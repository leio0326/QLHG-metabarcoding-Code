import pandas as pd

def fa2csv(input_file, output_file):
    with open(input_file, 'r') as f:
        fa = f.read()

    fa = fa.replace('\n>', 'row').replace(';\n', ',').replace('\n', '').replace('row', '\n').replace('>', '')
    # print(fa)
    with open(output_file, 'w') as f:
        f.write(fa)

    df = pd.read_csv(output_file, header=None)
    # print(df[0][1])
    # df[0] = np.arange(len(df))  # 使用 numpy 的 arange 函数生成从 0 开始的递增数字序列

    # # 设置列名
    df.columns = ['name', 'seq']  # 数据只有两列，第一列设置为 'id'，第二列设置为 'seq'

    df.to_csv(output_file, index=False)  # 写入 CSV 文件包含列名

if __name__ == "__main__":
    input_file = './data/101808.uni.fa'
    output_file = './data/101808.uni.csv'
    fa2csv(input_file, output_file)