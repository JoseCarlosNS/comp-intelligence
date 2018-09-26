# coding: utf-8
class Perceptron:

    'Classe que representa um perceptron'

    def __init__(self, pesos, bias = 0):
        self.pesos = pesos
        self.bias = bias

    # Função de ativação
    def ativacao(self, saida):
        if saida < 0:
            return 0
        else:
            return 1

    # Função combinatória do neurônio
    def somatorio(self, entradas):
        soma = self.bias * 1
        contador = 0
        for entrada in entradas:
            soma = soma + (entrada * self.pesos[contador])
        return soma

    def computar(self, entradas):
        return self.ativacao(self.somatorio(entradas))

# Testes:
p1 = Perceptron([1, 1], -1)
print(p1.computar([0, 0]))
print(p1.computar([0, 1]))
print(p1.computar([1, 0]))
print(p1.computar([1, 1]))
