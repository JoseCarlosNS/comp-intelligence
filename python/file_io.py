def ler_dataset(nome, separador = ' ', ignore_first_line=False):
    dados = []
    with open(nome, 'rt') as f:
        if ignore_first_line:
            f.readline()
        for linha in f:
            entradas = []
            vetor = linha.split(separador)
            for entrada in vetor:
                entradas = entradas + [int(entrada)]
            dados = dados + [entradas]
    return dados

def salvar_perceptron(nome, perceptron, separador='', rotulos=''):
    with open(nome, 'wt') as f:
