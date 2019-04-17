--tipos angebricos

data Temperatura = Frio | Calor
	deriving(Eq,Show)

data Estacao = Verao | Outono | Inverno | Primavera
	deriving(Eq,Show)

tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _ 	= Frio

data Funcionario  = Pessoa Nome Idade
	deriving(Eq,Show)

type Nome = String
type Idade = Int

andre :: Funcionario
andre  = Pessoa "Andre Du Bois" 28

pegaNome :: Funcionario -> Nome
pegaNome (Pessoa n i) = n

pegaIdade :: Funcionario -> Idade
pegaIdade (Pessoa n i) = i

data Forma = Circulo Float |Retangulo Float Float
	deriving(Eq,Show)

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = a*b

--LISTA 10
type Telefone = Int


data ItemDeLocadora = CD String String |DVD String String |Videos String
	deriving(Eq,Show)

data SocioLocadora = Socio Nome Telefone
	deriving(Eq,Show)

jader :: SocioLocadora
jader  = Socio "jader homo" 242424
	
type ItensDisponiveis = [ItemDeLocadora]

minhaLista :: ItensDisponiveis
minhaLista = [CD "jader""gay", DVD "patrick passou em""SO", Videos "Jader nao"]

type ItemALugado = (SocioLocadora,ItemDeLocadora)

alugado :: ItemALugado
alugado = (Socio "jader" 313131, CD "Aborta o"" pia")

type TodosALugados  = [ItemALugado]

itemP :: ItemDeLocadora
itemP = CD "aborta""pia"
listaDeALugados :: TodosALugados
listaDeALugados = [(Socio "jader" 313131, CD "Aborta o"" pia"),(Socio "jader" 313131, CD "Aborta o"" pia")]

alugarItem :: ItemDeLocadora -> SocioLocadora -> ItemALugado
alugarItem i s = (s,i)
