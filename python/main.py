import file_io, perceptron

data = file_io.ler_dataset('databases/numbers/zero.csv', ',', ignore_first_line=True)

p = perceptron.Perceptron(36)
p.aprender(data)
p.print_info()
