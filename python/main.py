from file_io import *
from util import *
from sklearn.neural_network import MLPClassifier
from sklearn.linear_model import Perceptron
from sklearn.metrics import classification_report, confusion_matrix

opcao = '1'
while opcao != '0':
    print_opcoes()
    opcao = input()
    if opcao == '1' or opcao == '2':
        dados = input_dados_treinamento()
        rotulos = dados[0]
        entradas = dados[1]
        saidas_desejadas = dados[2]

        dados_pp = pre_processamento(entradas, saidas_desejadas, 0.3)
        entrada_treino = dados_pp[0]
        entrada_teste = dados_pp[1]
        saida_treino = dados_pp[2]
        saida_teste = dados_pp[3]

        ruido = input('Deseja adicionar ruido? (s/n): ')
        if ruido == 's':
            ruido = input('Digite o número de bits de ruido: ')
            entrada_teste = gerar_ruido(entrada_teste, int(ruido))

        if opcao == '1':
            taxa_ap = float(input('Digite a taxa de aprendizado: '))
            max_epoch = int(input('Digite o # máximo de interações: '))

            perc = Perceptron(n_iter=max_epoch, eta0=taxa_ap)
            perc.fit(entrada_treino, saida_treino)

            predicoes = perc.predict(entrada_teste)
        else:
            cam_escond = input('Digite as qtds de neuronios em cada camada: ')
            cam_escond = cam_escond.split(',')
            cam_escond_aux = []
            for x in cam_escond:
                cam_escond_aux = cam_escond_aux + [int(x)]

            mlp = MLPClassifier(hidden_layer_sizes=cam_escond_aux)
            mlp.fit(entrada_treino, saida_treino)
            predicoes = mlp.predict(entrada_teste)

        print('Matriz de confusão: ')
        print(confusion_matrix(saida_teste, predicoes))
        print('Relatório: ')
        print(classification_report(saida_teste, predicoes))
