envel.gama <- function(modelo=fit.model,iden=0,nome=seq(along = model.matrix(modelo)[,1]),sim=100,conf=.90,res="D",mv=F,quad=T,maxit=20) {

#
# Descrição e detalhes:
# A saída será o gráfico de probabilidade normal com envelopes simulados para um ajuste da distribuição gama.
#
# A opção res="C" faz o gráfico de probabilidade meio-normal com envelopes simulados utilizando a distância de Cook,
# possibilitando a detecção de pontos simultaneamente aberrantes e/ou influentes.
#
# Atenção: a função não funcionará corretamente se o ajuste possuir offsets! Neste caso é preciso adaptá-la como foi
# feito na função envel.pois
#
# Os dados devem estar disponíveis pelo comando attach( ).
#
# Argumentos obrigatórios:
# modelo: deve-se informar o objeto onde está o ajuste do modelo, caso não seja informado, a função procurará
# 	  o ajuste no objeto fit.model;
# 
# Argumentos opcionais:
# iden: caso deseje, informe o número de observações que irá querer destacar. O padrão é não destacar ninguém (iden=0).
#	Qualquer valor que não seja um inteiro positivo (por ex., negativo ou decimal) fará com que a função pergunte
#	o número de pontos após a execução;
# nome: esse argumento só é utilizado caso seja destacado algum ponto no gráfico. Caso não seja informado nada, os pontos
#	identificados serão os números da ordem em que estão no banco de dados (os índices). Caso se queira, pode-se
#	informar um vetor de nomes ou de identificações alternativas. Obrigatoriamente esse vetor deve ter o mesmo
#	comprimento do banco de dados;
# sim: número de simulações para gerar a banda de confiança. Atkinson sugere um mínimo de 20 simulações.
#      O padrão é de 100;
# conf: nível de confiança do envelope. O padrão é de 90%;
# res: permite-se a escolha dos resíduos. As opções dos resíduos são: "Q" quantil (ver Dunn e Smyth, 1996), "D" componente
#      do desvio, "P" Pearson padronizado, "A" Anscombe, "W" Williams e "C" distância de Cook. A opção padrão é a "D";
# mv: o valor T (True) fará com se obtenha a estimativa de máxima verossimilhança (EMV) para o parâmetro de
#     dispersão. O valor F (False) indicará a escolha pela estimativa consistente pelo método dos momentos. O
#     padrão é F. Note que como a EMV é razoavelmente mais demorada para ser obtida, a função demorará mais
#     para rodar. Para obter a EMV a biblioteca MASS deve estar presente, no entanto não requer-se que seja
#     carregada previamente;
# quad: o padrão (quad=T, True) faz um gráfico quadrado, enquanto quad=F (False) faz um gráfico utilizando a área máxima
#       disponível;
# maxit: essa opção é utilizada nos ajustes de cada simulação e indica o máximo de iterações permitidas nos ajustes.
#	 O padrão é maxit=20.
#
# Autor: Frederico Zanqueta Poleto <fred@poleto.com>, arquivo disponível em http://www.poleto.com
#
# Referências:
# DUNN, K. P., and SMYTH, G. K. (1996). Randomized quantile residuals. J. Comput. Graph. Statist. 5, 1-10
#    [http://www.statsci.org/smyth/pubs/residual.html e http://www.statsci.org/smyth/pubs/residual.ps]
# MCCULLAGH, P. e NELDER, J. A. (1989). Generalized Linear Models. 2ª ed. Chapman and Hall, London.
# PAULA, G. A. (2003). Modelos de Regressão com apoio computacional. IME-USP, São Paulo. [Não publicado,
#    disponível em http://www.ime.usp.br/~giapaula/Book.pdf]
#
# Exemplos:
# envel.gama(ajuste,sim=1000,conf=.95,mv=T,maxit=50)
# envel.gama(ajuste,res="C")
#

if(class(modelo)[1] != "glm") {
	stop(paste("\nA classe do objeto deveria ser glm e nao ",class(modelo),"!!!\n"))
}
if(modelo$family[[1]] != "Gamma") {
	stop(paste("\nA familia do objeto deveria ser Gamma e nao ",modelo$family[[1]],"!!!\n"))
}

alfa<-(1-conf)/2
X <- model.matrix(modelo)
n <- nrow(X)
p <- ncol(X)
w <- modelo$weights
W <- diag(w)
H <- solve(t(X)%*%W%*%X)
H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
h <- diag(H)

#para evitar divisão por 0 ao studentizar os residuos, mas tentando manter o valor exagerado da alavanca
h[round(h,15)==1]<-0.999999999999999

m<-predict(modelo,type="response")
y<-modelo$y
if(mv==F) {
	fi <- (n-p)/sum((resid(modelo,type="response")/m)^2)
} else {
	library("MASS")
	fi <- 1/gamma.dispersion(modelo) #Função gamma.shape retorna phi do texto, gamma.shape$alpha=1/gamma.dispersion
}

if(res=="Q") {
	tipo<-"Resíduo Quantil"
	r<-qnorm( pgamma(y,fi,fi/m) )
} else {
	if(res=="D") {
		tipo<-"Resíduo Componente do Desvio"
		r<-resid(modelo,type="deviance")*sqrt(fi/(1-h))
	} else {
		if(res=="P") {
			tipo<-"Resíduo de Pearson Padronizado"
			r<-resid(modelo,type="pearson")*sqrt(fi/(1-h))
		} else {
			if(res=="A") {
				tipo<-"Resíduo de Anscombe"
				r<-3*sqrt(fi)*( y^(1/3) - m^(1/3) )/(m^(1/3))
			} else {
				if(res=="W") {
					tipo<-"Resíduo de Williams"
					r<-sign(y-m)*sqrt((1-h)*(( resid(modelo,type="deviance")*sqrt(fi/(1-h)) )^2)+(h*( resid(modelo,type="pearson")*sqrt(fi/(1-h)) )^2))
				} else {
					if(res=="C") {
						tipo<-"Distância de Cook"
						r<-(h/((1-h)*p))*((resid(modelo,type="pearson")/sqrt(1-h))^2)
					} else {
						stop(paste("\nVoce nao escolheu corretamente um dos residuos disponiveis!!!\n"))
					}
				}
			}
		}
	}
}

link<-modelo$family[[2]]

e <- matrix(0,n,sim)
e1 <- numeric(n)
e2 <- numeric(n)

if (is.null(version$language) == T) {
	#No S-Plus, a opção start é para entrar com o preditor linear
	pm<-predict(modelo)
} else {
	#No R, a opção start é para entrar com os coeficientes
	pm<-coef(modelo)
}
mu<-m
for(i in 1:sim) {
	resp <- rgamma(n,fi,fi/mu)
	if ( (is.null(version$language) == T && link == "Log: log(mu)") | (is.null(version$language) == F && link == "log") ) {
		fit <- glm(resp ~ X-1,family=Gamma(link=log),maxit=maxit,start=pm)
	} else {
		if ( (is.null(version$language) == T && link == "Inverse: 1/mu") | (is.null(version$language) == F && link == "inverse") ) {
			fit <- glm(resp ~ X-1,family=Gamma,maxit=maxit,start=pm)
		} else {
			if ( (is.null(version$language) == T && link == "Identity: mu") | (is.null(version$language) == F && link == "identity") ) {
				fit <- glm(resp ~ X-1,family=Gamma(link=identity),maxit=maxit,start=pm)
			} else {
				stop(paste("\nEsta funcao so aceita as ligacoes: canonica, log e identidade!!!\nLigacao ",link," desconhecida!!!\n"))
			}
		}
	}
	w <- fit$weights
	W <- diag(w)
	H <- solve(t(X)%*%W%*%X)
	H <- sqrt(W)%*%X%*%H%*%t(X)%*%sqrt(W)
	h <- diag(H)
	h[round(h,15)==1]<-0.999999999999999
	m <- predict(fit,type="response")
	y <- fit$y
	if(mv==F) {
		phi <- (n-p)/sum((resid(fit,type="response")/m)^2)
	} else {
		phi <- 1/gamma.dispersion(fit)
	}
	e[,i] <- 
	sort( if(res=="Q") {
		qnorm( pgamma(y/(m/phi),phi) )
	} else {
		if(res=="D") {
			resid(fit,type="deviance")*sqrt(phi/(1-h))
		} else {
			if(res=="P") {
				resid(fit,type="pearson")*sqrt(phi/(1-h))
			} else {
				if(res=="A") {
					3*sqrt(phi)*( y^(1/3) - m^(1/3) )/(m^(1/3))
				} else {
					if(res=="W") {
						sign(y-m)*sqrt((1-h)*(( resid(fit,type="deviance")*sqrt(phi/(1-h)) )^2)+(h*( resid(fit,type="pearson")*sqrt(phi/(1-h)) )^2))
					} else {
						if(res=="C") {
							(h/((1-h)*p))*((resid(fit,type="pearson")/sqrt(1-h))^2)
						} else {
							stop(paste("\nVoce nao escolheu corretamente um dos residuos disponiveis!!!\n"))
						}
					}
				}
			}
		}
	})
}

for(i in 1:n) {
	eo <- sort(e[i,])
	e1[i] <- quantile(eo,alfa)
	e2[i] <- quantile(eo,1-alfa)
}

med <- apply(e,1,median)

if(quad==T) {
	par(pty="s")
}
if(res=="C") {
	#Segundo McCullagh e Nelder (1989, pág.407) e Paula (2003, pág.57) deve-se usar qnorm((n+1:n+.5)/(2*n+1.125))
	#Segundo Neter et alli (1996, pág.597) deve-se usar qnorm((n+1:n-.125)/(2*n+0.5))
	qq<-qnorm((n+1:n+.5)/(2*n+1.125))
	plot(qq,sort(r),xlab="Quantil Meio-Normal",ylab=tipo, ylim=range(r,e1,e2), pch=16)
} else {
	qq<-qnorm((1:n-.375)/(n+.25))
	plot(qq,sort(r),xlab="Quantil da Normal Padrão",ylab=tipo, ylim=range(r,e1,e2), pch=16)
}
lines(qq,e1,lty=1)
lines(qq,e2,lty=1)
lines(qq,med,lty=2)
nome<-nome[order(r)]
r<-sort(r)
while ( (!is.numeric(iden)) || (round(iden,0) != iden) || (iden < 0) ) {
	cat("Digite o num.de pontos a ser identificado (0=nenhum) e <enter> para continuar\n")
	out <- readline()
	iden<-as.numeric(out)
}
if(iden>0) {identify(qq,r,n=iden,labels=nome)}
if(quad==T) {
	par(pty="m")
}
cat("Banda de ",conf*100,"% de confianca, obtida por ",sim," simulacoes.\n")
}
