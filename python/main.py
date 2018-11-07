from file_io import *
from util import *
from sklearn.neural_network import MLPClassifier
from sklearn.linear_model import Perceptron
import pandas as pd

opcao = 1
while opcao != 0:
    print_opcoes()
    opcao = input()
    print(opcao)
    if opcao == 1:
        dados = input_dados_treinamento()
        rotulos = dados[0]
        entradas = dados[1]
        saidas_desejadas = dados[2]

        dados_pp = pre_processamento(entradas, saidas_desejadas, 0.3)
        entrada_treino = dados_pp[0]
        entrada_teste = dados_pp[1]
        saida_treino = dados_pp[2]
        saida_teste = dados_pp[3]

        taxa_ap = input('Digite a taxa de aprendizado: ')
        max_epoch = input('Digite o # máximo de interações: ')

        perceptron = Perceptron(n_iter=max_epoch, eta0=taxa_ap)
        perceptron.fit(entrada_treino, saida_treino)
