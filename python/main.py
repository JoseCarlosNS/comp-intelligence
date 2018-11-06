from file_io import *
from perceptron import Perceptron
from sklearn.neural_network import MLPClassifier


data = ler_dataset('databases/numbers/zero.csv', ',', ignore_first_line=True)
p = Perceptron(36)
p.aprender(data)
p.print_info()
