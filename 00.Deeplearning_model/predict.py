import torch
import torch.nn as nn
from torch.utils.data import Dataset, DataLoader
import pandas as pd
import numpy as np


# 参数配置（与训练时一致）
class Config:
    batch_size = 64
    seq_length = 316  # 必须与训练时相同
    char_dim = 26  # 26字母 + 填充符'X'
    input_size = seq_length * char_dim  # 全连接输入维度


# 自定义数据集类（仅含数据）
class SequenceDataset(Dataset):
    def __init__(self, sequences):
        self.sequences = sequences

    def __len__(self):
        return len(self.sequences)

    def __getitem__(self, idx):
        return self.sequences[idx]


# 数据预处理函数（无标签处理）
def preprocess_data(csv_path):
    df = pd.read_csv(csv_path)

    # 字符编码（包含填充符 'X'）
    char_set = set('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
    char_to_idx = {char: i for i, char in enumerate(sorted(char_set))}

    encoded_seqs = []
    for seq in df['seq']:
        # 填充/截断到固定长度
        padded_seq = seq.ljust(Config.seq_length, 'X')[:Config.seq_length]
        # one-hot编码
        encoded = np.zeros((Config.seq_length, Config.char_dim), dtype=np.float32)
        for i, char in enumerate(padded_seq):
            encoded[i, char_to_idx[char]] = 1.0
        encoded_seqs.append(encoded.flatten())  # 展平为1D向量

    # 转换为张量
    X = torch.tensor(np.array(encoded_seqs), dtype=torch.float32)
    return X


# 定义模型结构（必须与训练时一致）
class FCNet(nn.Module):
    def __init__(self):
        super(FCNet, self).__init__()
        self.net = nn.Sequential(
            nn.Linear(Config.input_size, 128),
            nn.ReLU(),
            nn.Dropout(0.5),
            nn.Linear(128, 64),
            nn.ReLU(),
            nn.Linear(64, 1),
            nn.Sigmoid()  # 输出概率值
        )

    def forward(self, x):
        return self.net(x)


# 预测函数（返回二分类结果）
def predict(model, loader):
    model.eval()
    all_preds = []
    all_probs = []
    with torch.no_grad():
        for inputs in loader:
            outputs = model(inputs)
            # probs = torch.sigmoid(outputs)
            probs = outputs
            all_probs.extend(probs.cpu().numpy())
            preds = (outputs > 0.5).int().flatten()  # 阈值0.5分类
            all_preds.extend(preds.cpu().numpy())

    return all_preds, all_probs  # 0/1列表


# 主流程
def main(csv_path, output_path):
    # 数据预处理
    X = preprocess_data(csv_path)

    # 创建DataLoader
    test_dataset = SequenceDataset(X)
    test_loader = DataLoader(test_dataset, batch_size=Config.batch_size)

    # 加载模型
    model = FCNet()
    model.load_state_dict(torch.load("./models/best_model_0313.pth"))

    # 执行预测
    predictions,probs = predict(model, test_loader)

    # 保存预测结果到CSV
    df = pd.read_csv(csv_path)
    df['prediction'] = predictions
    df['prob'] = probs
    df.to_csv(output_path, index=False)
    print(f"预测结果已保存至 {output_path}")


if __name__ == "__main__":
    input_file = './data/101808.uni.csv'
    output_file = './data/101808_pred.csv'
    main(input_file, output_file)  # 替换为实际路径