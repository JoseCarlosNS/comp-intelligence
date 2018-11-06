import file_io, perceptron,random


# Função que gera um vetor com 'num_entradas' de valores {0, 1}
# que podem ser usados para alimentar uma experiencia
def gerar_entradas(num_entradas):
    entradas = []
    for x in range(2 ** num_entradas):
        entrada = []
        if x == 0:
            entrada = [0] * num_entradas
        else:
            entrada = entradas[x - 1].copy()
        for y in range(num_entradas):
            if x % (2 ** y) == 0:
                index = num_entradas - 1 - y
                if entrada[index] == 0:
                    entrada[index] = 1
                else:
                    entrada[index] = 0
        entradas = entradas + [entrada]
    entradas.reverse()
    return entradas

# Função que toma os dados de entrada e retorna as entradas com uma dada qtd
# de entradas alteradas como ruido
def gerar_ruido(entradas, ruido):
    for entrada in entradas:
        index = random.randint(0, len(entrada) - 1)
        if entrada[index] == 0:
            entrada[index] = 1
        else:
            entrada[index] = 0
    return
