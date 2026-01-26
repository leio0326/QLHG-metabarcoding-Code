使用神经网络模型进行快速分类的流程如下：
1.将fasta文件转换为csv文件
	修改fa2csv.py中的input_file与output_file，然后执行命令：
	python fa2csv.py
	结果文件将保存在output_file指定的路径下
2.预测序列对应的类别（昆虫 or 非昆虫）
	修改predict.py中的input_file与output_file，然后执行命令：
	python predict.py
	预测的结果将以csv文件保存在output_file指定的路径下
3.将预测结果转换为fasta文件，并按照类别分到不同的文件中
	修改csv2fa.py中的input_file，然后执行命令：
	python csv2fa.py
	结果文件将保存在data文件夹下，并按照昆虫/非昆虫分为两个fasta文件