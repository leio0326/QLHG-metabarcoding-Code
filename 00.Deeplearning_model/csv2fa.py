import pandas as pd

def df_to_fasta_by_tag(df, output_prefix):
    """
    将包含序列的DataFrame按标签分类保存为FASTA文件

    参数：
    df : pandas.DataFrame
        包含三列的DataFrame（id, seq, tag）
    output_prefix : str, 可选
        输出文件前缀，默认为"tag"
    """
    # 按tag分组
    grouped = df.groupby('prediction')

    # 遍历每个标签组
    for tag, group in grouped:
        # 生成文件名
        if int(tag) == 1:
            filename = f"{output_prefix}_insecta.fasta"
        elif int(tag) == 0:
            filename = f"{output_prefix}_noninsecta.fasta"

        # 写入FASTA格式
        with open('./data/' + filename, 'w') as f:
            for _, row in group.iterrows():
                # 写入序列头
                f.write(f">{row['name']}\n")
                # 写入序列（每行80个字符为标准格式）
                seq = row['seq']
                formatted_seq = '\n'.join([seq[i:i + 80] for i in range(0, len(seq), 80)])
                f.write(f"{formatted_seq}\n")


# 使用示例
if __name__ == "__main__":
    input_file = './data/101808_pred.csv'
    
    df = pd.read_csv(input_file)
    del df['prob']

    name = input_file.split('/')[2].split('.')[0]

    # 调用转换函数
    df_to_fasta_by_tag(df, output_prefix=name)

