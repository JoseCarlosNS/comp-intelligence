# coding: utf-8

class Perceptron:

    'Classe que representa um perceptron'

    # Construtor, perceba que o peso do bias é o primeiro elemento
    # da lista de pesos
    def __init__(self, pesos = [], bias = 0):
        self.pesos = [bias] + pesos

    # Função de ativação
    def ativacao(self, saida):
        if saida < 0:
            return 0
        else:
            return 1

    # Função combinatória do neurônio
    def somatorio(self, entradas):
        soma = self.pesos[0] * 1
        contador = 1
        for entrada in entradas:
            soma = soma + (entrada * self.pesos[contador])
            contador = contador + 1
        return soma

    # Função que calcula a saída do perceptron dado um vetor de entradas
    def computar(self, entradas):
        return self.ativacao(self.somatorio(entradas))

    # Função de aprendizado
    # Os dados_treino é uma lista onde cada elemento é um vetor de números que
    # representam as entradas e o último elemento é a saída desejada
    def aprender(self, dados_treino, taxa_ap = 0.01, max_epoch = 1000):
        epoch = 0
        num_entradas = len(dados_treino[0]) - 1
        self.pesos = [0] * (num_entradas + 1)
        while 1:
            errop = 0
            epoch = epoch + 1
            # print(self.pesos)
            for x in dados_treino:
                entradas = x[:num_entradas]
                saida_desejada = x[num_entradas]
                saida_perceptron = self.somatorio(entradas)
                if saida_desejada != self.ativacao(saida_perceptron):
                    errop = 1
                    erro = taxa_ap * (saida_desejada - saida_perceptron)
                    # print("Erro: ", erro)
                    for w in range(len(self.pesos)):
                        novo_peso = self.pesos[w] + erro
                        # print(novo_peso)
                        self.pesos[w] = novo_peso
            if not errop or epoch == max_epoch:
                break

    # Função que printa informações sobre o perceptron:
    def print_info(self):
        print("""Informações sobre o perceptron:
    Pesos: {}
    Bias: {}""".format(self.pesos[1:], self.pesos[0]))


# Testes:
p1 = Perceptron()
treino = [[0,0,0],[0,1,0],[1,0,1],[1,1,0]]
p1.aprender(treino, 0.1)
p1.print_info()
print(p1.computar([0, 0]))
print(p1.computar([0, 1]))
print(p1.computar([1, 0]))
print(p1.computar([1, 1]))
