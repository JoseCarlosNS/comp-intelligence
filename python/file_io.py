import perceptron

def ler_dataset(nome, separador = ' ', ignore_first_line=False):
    dados = []
    try:
        with open(nome, 'rt') as f:
            if ignore_first_line:
                f.readline()
            for linha in f:
                entradas = []
                vetor = linha.split(separador)
                for entrada in vetor:
                    entradas = entradas + [int(entrada)]
                dados = dados + [entradas]
    except:
        print('Erro na leitura do arquivo!')

    return dados


def salvar_perceptron(nome, perceptron, separador=',', rotulos=''):
    try:
        with open(nome, 'wt') as f:
            if rotulos:
                for rotulo in rotulos:
                    f.write('{},'.format(rotulo))
                f.write('bias\n')
            for peso in perceptron.pesos:
                f.write("{},".format(peso))
            f.write(str(perceptron.bias) + '\n')
    except:
        print('Erro na leitura do arquivo!')

    return


def carregar_perceptron(nome, tem_rotulos=False, separador=','):
    try:
        with open(nome, 'rt') as f:
            rotulos = ''
            if tem_rotulos:
                rotulos = f.readline()
                rotulos = rotulos.split(separador)
                rotulos.pop()
            pesos_temp = f.readline()
            pesos_temp = pesos_temp.split(separador)
            pesos = []
            for peso in pesos_temp:
                pesos = pesos + [float(peso)]
            bias = pesos.pop()
            percept = perceptron.Perceptron(len(pesos), pesos, rotulos, bias)
            return percept
    except:
        print('Erro na leitura do arquivo!')

    return
