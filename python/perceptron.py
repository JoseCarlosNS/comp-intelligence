# coding: utf-8

class Perceptron:
    'Classe que representa um perceptron'

    # Construtor, perceba que o peso do bias é o primeiro elemento
    # da lista de pesos
    def __init__(self, num_entradas, bias=0):
        self.pesos = [0] * num_entradas
        self.bias = bias

    # Função de ativação
    def ativacao(self, saida):
        if saida <= 0:
            return 0
        else:
            return 1

    # Função de calcular novo peso
    def novo_peso(self, peso_atual, erro, taxa_ap, entrada):
        np = peso_atual + taxa_ap * erro * entrada
        return np

    # Função combinatória do neurônio
    def somatorio(self, entradas):
        soma = self.bias
        contador = 0
        for entrada in entradas:
            soma = soma + (entrada * self.pesos[contador])
            contador = contador + 1
        return soma

    # Função que calcula a saída do perceptron dado um vetor de entradas
    def computar(self, entradas):
        soma = self.somatorio(entradas)
        at = self.ativacao(soma)
        print("Somatório: {}. Ativação: {}".format(soma, at))
        return at

    # Função que calcula o erro do perceptron
    def erro(self, saida_desejada, saida_atual):
        e = saida_desejada - saida_atual
        return e

    # Função de aprendizado
    # Os dados_treino é uma lista onde cada elemento é um vetor de números que
    # representam as entradas e o último elemento é a saída desejada
    def aprender(self, dados_treino, taxa_ap=0.01, max_epoch=1000):
        epoch = 0
        num_entradas = len(dados_treino[0]) - 1
        while 1:
            errop = 0
            for x in dados_treino:
                entradas = x[:num_entradas]
                saida_desejada = x[num_entradas]
                soma = self.somatorio(entradas)
                saida_perceptron = self.ativacao(soma)
                if saida_desejada != saida_perceptron:
                    errop = 1
                    e = self.erro(saida_desejada, saida_perceptron)
                    cont = 0
                    self.bias = self.novo_peso(self.bias, e, taxa_ap, 1)
                    for w in range(len(self.pesos)):
                        print(x)
                        np = self.novo_peso(self.pesos[w], e, taxa_ap, entradas[w])
                        self.pesos[w] = np
                        cont = cont + 1
                epoch = epoch + 1
                if errop:
                    break
            if not errop or epoch == max_epoch:
                break

    # Função que printa informações sobre o perceptron:
    def print_info(self):
        print("""Informações sobre o perceptron:
    Pesos: {}
    Bias: {}""".format(self.pesos, self.bias))
        