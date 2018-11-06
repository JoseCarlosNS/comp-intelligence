# coding: utf-8
import file_io

class Perceptron:
    'Classe que representa um perceptron'

    # Construtor, perceba que o peso do bias é o primeiro elemento
    # da lista de pesos
    def __init__(self, num_entradas, pesos=[], rotulos = '', bias=0):
        self.num_entradas = num_entradas

        if pesos:
            self.pesos = pesos
        else:
            self.pesos = [0] * num_entradas

        self.bias = bias
        self.rotulos = rotulos

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
        return at

    # Função que calcula o erro do perceptron
    def erro(self, saida_desejada, saida_atual):
        e = saida_desejada - saida_atual
        return e

    # Função que atualiza os pesos do perceptron de acordo com
    # o algoritmo de aprendizado
    def aprender(self, entradas, saidas_desejadas, taxa_ap=0.01, max_epoch=1000):
        epoch = 0
        while 1:
            errop = 0
            for entrada, saida_desejada in zip(entradas, saidas_desejadas):
                soma = self.somatorio(entrada)
                saida_perceptron = self.ativacao(soma)
                if saida_desejada != saida_perceptron:
                    errop = 1
                    e = self.erro(saida_desejada, saida_perceptron)
                    cont = 0
                    self.bias = self.novo_peso(self.bias, e, taxa_ap, 1)
                    for w in range(len(self.pesos)):
                        np = self.novo_peso(self.pesos[w], e, taxa_ap, entrada[w])
                        self.pesos[w] = np
                        cont = cont + 1
                epoch = epoch + 1
                if errop:
                    break
            if not errop or epoch >= max_epoch:
                break

    # Função que printa informações sobre o perceptron:
    def print_info(self):
        print("""Informações sobre o perceptron:
    Rotulos: {}
    Pesos: {}
    Bias: {}""".format(self.rotulos, self.pesos, self.bias))
