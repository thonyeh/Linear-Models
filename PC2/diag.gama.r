diag.gama <- function(modelo=fit.model,iden=c(0,0,0,0,0,0,0,0),nome=seq(along = model.matrix(modelo)[,1]),res="D",del=F,mv=F,maxit=20) {

#
# Descrição e detalhes:
# A saída terá oito gráficos:
# 1º) Influência na Locação. O gráfico feito é das distâncias de Cook contra os valores ajustados. Utilizou-se o critério
#     de destacar observações maiores do que duas vezes a média de todas as distâncias obtidas;
# 2º) Influência Locação/Escala. A medida C, que é um aperfeiçoamento do DFFIT e também é conhecida como distância de Cook
#     modificada, foi utilizada para medir a influência das observações nos parâmetros de locação e escala. O critério
#     foi o de destacar observações maiores do que duas vezes a média de todas as distâncias obtidas;
# 3º) Influência Local. A influência local consiste em procurar pontos que sob pequenas perturbações causam variações
#     muito grandes nos resultados. O dmax é o autovetor que corresponde ao maior autovalor da matriz do processo de
#     perturbações. Para maiores detalhes veja Paula (2003, págs.50-54 e 65-66). O critério foi o de destacar observações
#     maiores do que duas vezes a média de todos os dmax's. Na influência local utiliza-se a matriz de informação de Fisher
#     observada. Como estamos utilizando a matriz esperada, os resultados obtidos são apenas aproximados se a ligação
#     utilizada não for a canônica;
# 4º) Função de Ligação. O gráfico feito é do preditor linear ajustado (eta) contra a variável dependente ajustada (z).
#     Segundo McCullagh e Nelder (1989, pág.401), o padrão esperado é de uma linha reta. Para funções de ligação da
#     família potência uma curvatura dos pontos acima da reta sugere uma ligação de uma potência maior que a utilizada,
#     enquanto que uma curvatura abaixo da reta uma potência menor. Conforme sugerido, é adicionado uma reta suavizada
#     pelo método lowess robusto e também uma linha tracejada com inclinação de 45º. O método deve ser utilizado com
#     cautela, uma vez que por ex.para a binomial ele não é informativo;
# 5º) Pontos Alavanca 1. Para os MLG's a matriz H=sqrt(W)%*%X%*%solve(t(X)%*%W%*%X)%*%t(X)%*%sqrt(W) é interpretada como
#     sendo a matriz de projeção da solução de mínimos quadrados da regressão linear de z contra X com pesos W. Assim,
#     sugere-se utilizar h=diag(H) para detectar a presença de pontos alavanca no modelo de regressão normal ponderado. Um
#     ponto é considerado alavanca (leverage) quando este exerce uma forte influência no seu valor ajustado. O critério foi
#     o de destacar observações maiores do que duas vezes a média de todos os h’s, que nesse caso resume-se a duas vezes
#     o número de parâmetros do modelo, dividido pelo número de observações.
#     A medida de alavanca h depende da ligação através dos pesos. Como o gráfico de alavanca é feito dos h's contra as
#     médias, temos uma idéia do que esperar dependendo da ligação escolhida.
#	Na ligação log, o peso é constante (igual a 1).
#	Na ligação canônica (inversa, 1/mu) o peso é a média ao quadrado (mu^2).
#	Na ligação identidade (mu) o peso é o inverso da média ao quadrado (1/mu^2).
# 6º) Pontos Alavanca 2. Comentamos no quinto gráfico que a medida h tem uma forte dependência dos pesos conforme a
#     ligação escolhida. Assim, sugerimos uma medida h modificada que se dá por hm=diag(HM), em que
#     HM=X%*%solve(t(X)%*%W%*%X)%*%t(X). A idéia por trás dela é de tentar eliminar a forte dependência com os pesos e
#     facilitar a detecção dos pontos alavanca. Obviamente a medida não elimina toda a dependência, uma vez que HM ainda
#     depende de W, mas podemos utilizá-la adicionalmente à medida h já tradicionalmente sugerida. Vale notar que se for
#     escolhida uma ligação em que W seja uma matriz identidade, então h=hm. Hosmer e Lemeshow (2000, págs.169 a 173)
#     discutem as duas medidas para a regressão logística. Parece não existir estudos muito aprofundados de hm, mas
#     Hosmer e Lemeshow discutem críticas feitas por Pregibon à medida h com relação a dependência de W e críticas feitas
#     por Lesaffre em tentar ignorar a informação contida em W. Por fim, sugerem que ambas sejam utilizadas com cautela;
# 7º) Pontos Aberrantes. Um ponto é aberrante (discrepante, outlier) se o seu valor estiver mal ajustado pelo modelo.
#     Adicionamos linhas tracejadas em -2 e 2. Assim, se os resíduos escolhidos forem aproximadamente normais,
#     esperamos que cerca de 5% dos pontos possam estar um pouco fora desses limites. Mesmo sem normalidade, o gráfico
#     serve para inspecionar valores mal ajustados. Deve-se no entando tomar cuidado pois sabemos que por ex.o resíduo
#     de Pearson é assimétrico. Esse gráfico serve como indicação para detectar valores aberrantes marginalmente.
#     Devido o desconhecimento da distribuição dos resíduos e se o objetivo for detectar valores conjuntamente aberrantes
#     deve-se construir o gráfico de envelopes;
# 8º) Função de Variância. McCullagh e Nelder (1989, pág.400) sugere o gráfico dos resíduos absolutos contra os valores
#     ajustados (ou contra os valores ajustados transformados em escala constante) para checar se a função de variância
#     adotada é adequada. O padrão esperado é de não encontrarmos nenhuma tendência. Funções de variância erradas irão
#     resultar em tendências dos resíduos com a média. Tendências positivas indicam que a função de variância está
#     crescendo muito devagar com a média, então deve-se aumentar a potência (no caso de uma função de variância da
#     família potência). Uma linha suavizada pelo método lowess robusto é adicionada para ajudar na procura de tendências.
#
# Os dados devem estar disponíveis pelo comando attach( ).
#
# Argumentos obrigatórios:
# modelo: deve-se informar o objeto onde está o ajuste do modelo com distribuição gama, caso não seja informado, a
# 	  função procurará o ajuste no objeto fit.model;
#
# Argumentos opcionais:
# iden: caso deseje, informe o número de observações que irá querer destacar em cada gráfico. O vetor deve conter 8
#	posições de números inteiros. A ordem que deve ser informada é a mesma em que os gráficos são feitos. Os
#	componentes do vetor iguais a 0 indicam que não se quer que identifique pontos, se for um inteiro positivo irá
#	automaticamente nos gráficos respectivos permitir que identifiquemos o número de pontos solicitados e qualquer
#	outro valor (negativo ou decimal) parar nos gráficos e solicitar que especifiquemos o número de pontos a ser
#	destacado. O padrão é c(0,0,0,0,0,0,0,0) caso não se entre com nada e c(-1,-1,-1,-1,-1,-1,-1,-1) caso se entre
#	com qualquer coisa que não seja um vetor de 8 posições, como por ex.-1;
# nome: esse argumento só é utilizado caso algum dos componentes do vetor da opção iden não seja 0. Caso não seja
#	informado nada, os pontos identificados serão os números da ordem em que estão no banco de dados (índices).
#	Caso se queira, pode-se informar um vetor de nomes ou de identificações alternativas. Obrigatoriamente
#	esse vetor deve ter o mesmo comprimento do banco de dados;
# res: permite-se a escolha dos resíduos que serão utilizados nos gráficos de pontos aberrantes, da função
#      de ligação e na medida de influência na locação e na escala. As opções dos resíduos são: "Q" quantil (ver Dunn e
#      Smyth, 1996), "D" componente do desvio, "P" Pearson padronizado, "A" Anscombe e "W" Williams. A opção padrão é a "D";
# del: se for T (True) fará com que a função calcule o resíduo da i-ésima observação do ajuste que foi feito com
#      ela 'deletada' do banco de dados. Portanto, essa opção irá fazer com que o tempo de processamento da função
#      cresça proporcionalmente ao número de observações no banco de dados. O valor F (False) fará com que
#      se calcule os resíduos do próprio ajuste (deleted residuals). O padrão é F;
# mv: o valor T (True) fará com se obtenha a estimativa de máxima verossimilhança (EMV) para o parâmetro de
#     dispersão. O valor F (False) indicará a escolha pela estimativa consistente pelo método dos momentos. O
#     padrão é F. Note que como a EMV é razoavelmente mais demorada para ser obtida, a função demorará mais
#     para rodar. Para obter a EMV a biblioteca MASS deve estar presente, no entanto não requer-se que seja
#     carregada previamente;
# maxit: essa opção só é utilizada se del=T. Ela é utilizada nos ajustes feitos sem as observações e indica o máximo
#	 de iterações permitidas nos ajustes. McCullagh e Nelder (1989) sugerem que aproximações de 1 iteração
#	 podem ser utilizadas. O padrão é maxit=20.
#
# A função retorna os seguintes valores: ResQuantil, ResCompDesv, ResAnscombe, ResPearsonStd, ResWilliams, Di, Ci, Dmax e h.
#
# Autor: Frederico Zanqueta Poleto <fred@poleto.com>, arquivo disponível em http://www.poleto.com
#
# Referências:
# DUNN, K. P., and SMYTH, G. K. (1996). Randomized quantile residuals. J. Comput. Graph. Statist. 5, 1-10
#    [http://www.statsci.org/smyth/pubs/residual.html e http://www.statsci.org/smyth/pubs/residual.ps]
# HOSMER, D. W. e LEMESHOW, S. (2000). Applied Logistic Regression. John Wiley & Sons, New York.
# MCCULLAGH, P. e NELDER, J. A. (1989). Generalized Linear Models. 2ª ed. Chapman and Hall, London.
# PAULA, G. A. (2003). Modelos de Regressão com apoio computacional. IME-USP, São Paulo. [Não publicado,
#    disponível em http://www.ime.usp.br/~giapaula/Book.pdf]
#
# Exemplos:
# diag.gama(ajuste,iden=c(1,5,2,0,4,3,0,0),nome=estados)
# diag.gama(ajuste,iden=-1)
#

if(class(modelo)[1] != "glm") {
	stop(paste("\nA classe do objeto deveria ser glm e nao ",class(modelo),"!!!\n"))
}
if(modelo$family[[1]] != "Gamma") {
	stop(paste("\nA familia do objeto deveria ser Gamma e nao ",modelo$family[[1]],"!!!\n"))
}

if(length(iden)<8) {
	iden<-c(-1,-1,-1,-1,-1,-1,-1,-1)
}

X <- model.matrix(modelo)
n <- nrow(X)
p <- ncol(X)
w <- modelo$weights
W <- diag(w)
Fis <- t(X)%*%W%*%X
V <- solve(Fis)
H <- sqrt(W)%*%X%*%V%*%t(X)%*%sqrt(W)
h <- diag(H)

#para evitar divisão por 0 ao studentizar os residuos, mas tentando manter o valor exagerado da alavanca
h[round(h,15)==1]<-0.999999999999999

y <- modelo$y
m <- predict(modelo,type="response")
mut <- log(m) #McCullagh e Nelder (1989), pág.398 está 2*log(m), mas resolvendo a integral para obter uma transformação constante pelo Maple obtive log(m)
pl <- predict(modelo)
adj <- pl+residuals(modelo,type="working") #variável dependente ajustada

if(mv==F) {
	fi <- (n-p)/sum((resid(modelo,type="response")/m)^2)
} else {
	library("MASS")
	fi <- 1/gamma.dispersion(modelo)
}
rp <- resid(modelo,type="pearson")*sqrt(fi)

link<-modelo$family["link"]
if(del==F) {
	ts <- rp/sqrt(1-h)
	td <- resid(modelo,type="deviance")*sqrt(fi/(1-h))
	ra <- 3*sqrt(fi)*( y^(1/3) - m^(1/3) )/(m^(1/3))
	rq <- qnorm( pgamma(y,fi,fi/m) )
	di <- (h/((1-h)*p))*(ts^2)
	rw <- sign(y-m)*sqrt((1-h)*(td^2)+(h*ts^2))
} else {
	tdi <- numeric(n)
	rai <- numeric(n)
	rqi <- numeric(n)
	tsi <- numeric(n)
	dii <- numeric(n)
	rwi <- numeric(n)
	if (is.null(version$language) == T) {
		#No S-Plus, a opção start é para entrar com o preditor linear
		pm<-predict(modelo)
	} else {
		#No R, a opção start é para entrar com os coeficientes
		pm<-coef(modelo)
	}
	for(i in 1:n) {
		if ( (is.null(version$language) == T && link == "Log: log(mu)") | (is.null(version$language) == F && link == "log") ) {
			fiti <- glm(y ~ X -1,family=Gamma(link=log),subset=-i,maxit=maxit,start=pm)
		} else {
			if ( (is.null(version$language) == T && link == "Inverse: 1/mu") | (is.null(version$language) == F && link == "inverse") ) {
				fiti <- glm(y ~ X -1,family=Gamma,subset=-i,maxit=maxit,start=pm)
			} else {
				if ( (is.null(version$language) == T && link == "Identity: mu") | (is.null(version$language) == F && link == "identity") ) {
					fiti <- glm(y ~ X -1,family=Gamma(link=identity),subset=-i,maxit=maxit,start=pm)
				} else {
					stop(paste("\nEsta funcao so aceita as ligacoes: canonica, log e identidade!!!\nLigacao ",link," desconhecida!!!\n"))
				}
			}
		}
		Xi <- X[-i,]
		wi <- fiti$weights
		Wi <- diag(wi)
		Fi <- t(Xi)%*%Wi%*%Xi
		Vi <- solve(Fi)
		if(mv==F) {
			fii <- ((n-1)-p)/sum((resid(fiti,type="response")/(fitted(fiti)))^2)
		} else {
			fii <- 1/gamma.dispersion(fiti) #Função gamma.shape retorna phi do texto, gamma.shape$alpha=1/gamma.dispersion
		}
		yi <- y[i]
		mi <- predict(fiti,as.data.frame(X),type="response")[i]
		if ( (is.null(version$language) == T && link == "Log: log(mu)") | (is.null(version$language) == F && link == "log") ) {
			hi <- t(as.matrix(X[i,]))%*%Vi%*%as.matrix(X[i,])
		} else {
			if ( (is.null(version$language) == T && link == "Inverse: 1/mu") | (is.null(version$language) == F && link == "inverse") ) {
				hi <- (mi^2)*t(as.matrix(X[i,]))%*%Vi%*%as.matrix(X[i,])
			} else {
				if ( (is.null(version$language) == T && link == "Identity: mu") | (is.null(version$language) == F && link == "identity") ) {
					hi <- (mi^(-2))*t(as.matrix(X[i,]))%*%Vi%*%as.matrix(X[i,])
				} else {
					stop(paste("\nEsta funcao so aceita as ligacoes: canonica, log e identidade!!!\nLigacao ",link," desconhecida!!!\n"))
				}
			}
		}
		tdi[i] <- sign(yi-mi)*sqrt(2*((yi/mi)-log(yi/mi)-1)*(fii/(1+hi)))
		rai[i] <- 3*sqrt(fii)*( yi^(1/3) - mi^(1/3) )/(mi^(1/3))
		rqi[i] <- qnorm( pgamma(yi,fii,fii/mi) )
		tsi[i] <- ((yi-mi)*sqrt(fii/(1+hi)))/mi
		dii[i] <- (fi/p)*t(fiti$coef-modelo$coef)%*%Fis%*%(fiti$coef-modelo$coef)
		rwi[i] <- sign(yi-mi)*sqrt((1-hi)*(tdi[i]^2)+(hi*tsi[i]^2))
	}
	td<-tdi
	ra<-rai
	rq<-rqi
	ts<-tsi
	di<-dii
	rw<-rwi
}
A <- diag(rp)%*%H%*%diag(rp)
dmax <- abs(eigen(A)$vec[,1]/sqrt(eigen(A)$val[1]))

if(res=="Q") {
	tipor<-"Resíduo Quantil"
	r<-rq
} else {
	if(res=="D") {
		tipor<-"Resíduo Componente do Desvio"
		r<-td
	} else {
		if(res=="P") {
			tipor<-"Resíduo de Pearson Padronizado"
			r<-ts
		} else {
			if(res=="A") {
				tipor<-"Resíduo de Anscombe"
				r<-ra
			} else {
				if(res=="W") {
					tipor<-"Resíduo de Williams"
					r<-rw
				} else {
					stop(paste("\nVoce nao escolheu corretamente um dos residuos disponiveis!!!\n"))
				}
			}
		}
	}
}
ci <- sqrt( ((n-p)*h) / (p*(1-h)) )*abs(r)

if ( (is.null(version$language) == T && link == "Log: log(mu)") | (is.null(version$language) == F && link == "log") ) {
	hm <- h
} else {
	if ( (is.null(version$language) == T && link == "Inverse: 1/mu") | (is.null(version$language) == F && link == "inverse") ) {
		hm <- h/(m^2)
	} else {
		if ( (is.null(version$language) == T && link == "Identity: mu") | (is.null(version$language) == F && link == "identity") ) {
			hm <- h/(m^(-2))
		} else {
			stop(paste("\nEsta funcao so aceita as ligacoes: canonica, log e identidade!!!\nLigacao ",link," desconhecida!!!\n"))
		}
	}
}

par(mfrow=c(2,4))

plot(m,di,xlab="Valor Ajustado", ylab="Distância de Cook",main="Influência na Locação", ylim=c(0,max(di,2*mean(di))), pch=16)
abline(2*mean(di),0,lty=2)
while ( (!is.numeric(iden[1])) || (round(iden[1],0) != iden[1]) || (iden[1] < 0) ) {
	cat("Digite o num.de pontos a ser identificado (0=nenhum) e <enter> para continuar\n")
	out <- readline()
	iden[1]<-as.numeric(out)
}
if(iden[1]>0) {identify(m,di,n=iden[1],labels=nome)}

plot(m,ci,xlab="Valor Ajustado", ylab="Distância de Cook Modificada",main="Influência Locação/Escala", ylim=c(0,max(ci,2*mean(ci))), pch=16)
abline(2*mean(ci),0,lty=2)
while ( (!is.numeric(iden[2])) || (round(iden[2],0) != iden[2]) || (iden[2] < 0) ) {
	cat("Digite o num.de pontos a ser identificado (0=nenhum) e <enter> para continuar\n")
	out <- readline()
	iden[2]<-as.numeric(out)
}
if(iden[2]>0) {identify(m,ci,n=iden[2],labels=nome)}

plot(m,dmax,xlab="Valor Ajustado", ylab="dmax",main="Influência Local", ylim=c(0,max(dmax,2*mean(dmax))), pch=16)
abline(2*mean(dmax),0,lty=2)
while ( (!is.numeric(iden[3])) || (round(iden[3],0) != iden[3]) || (iden[3] < 0) ) {
	cat("Digite o num.de pontos a ser identificado (0=nenhum) e <enter> para continuar\n")
	out <- readline()
	iden[3]<-as.numeric(out)
}
if(iden[3]>0) {identify(m,dmax,n=iden[3],labels=nome)}

plot(adj,pl,xlab="Variável Dependente Ajustada",ylab="Preditor Linear Ajustado",main="Função de Ligação", pch=16)
lines(lowess(adj,pl))
abline(a=0,b=1,lty=2)
while ( (!is.numeric(iden[4])) || (round(iden[4],0) != iden[4]) || (iden[4] < 0) ) {
	cat("Digite o num.de pontos a ser identificado (0=nenhum) e <enter> para continuar\n")
	out <- readline()
	iden[4]<-as.numeric(out)
}
if(iden[4]>0) {identify(adj,pl,n=iden[4],labels=nome)}

plot(m,h,xlab="Valor Ajustado",ylab="Medida h",main="Pontos Alavanca 1",ylim=c(0,max(h,2*p/n)),pch=16)
abline(2*p/n,0,lty=2)
while ( (!is.numeric(iden[5])) || (round(iden[5],0) != iden[5]) || (iden[5] < 0) ) {
	cat("Digite o num.de pontos a ser identificado (0=nenhum) e <enter> para continuar\n")
	out <- readline()
	iden[5]<-as.numeric(out)
}
if(iden[5]>0) {identify(m,h,n=iden[5],labels=nome)}

plot(m,hm,xlab="Valor Ajustado",ylab="Medida h Modificada",main="Pontos Alavanca 2",ylim=c(0,max(hm,2*mean(hm))),pch=16)
abline(2*mean(hm),0,lty=2)
while ( (!is.numeric(iden[6])) || (round(iden[6],0) != iden[6]) || (iden[6] < 0) ) {
	cat("Digite o num.de pontos a ser identificado (0=nenhum) e <enter> para continuar\n")
	out <- readline()
	iden[6]<-as.numeric(out)
}
if(iden[6]>0) {identify(m,hm,n=iden[6],labels=nome)}

plot(m,r,xlab="Valor Ajustado",ylab=tipor,main="Pontos Aberrantes", ylim=c(min(r)-1,max(r)+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
while ( (!is.numeric(iden[7])) || (round(iden[7],0) != iden[7]) || (iden[7] < 0) ) {
	cat("Digite o num.de pontos a ser identificado (0=nenhum) e <enter> para continuar\n")
	out <- readline()
	iden[7]<-as.numeric(out)
}
if(iden[7]>0) {identify(m,r,n=iden[7],labels=nome)}

plot(m,abs(r),xlab="Valor Ajustado",ylab=paste(tipor," Absoluto",sep=""),main="Função de Variância", pch=16)
lines(lowess(m,abs(r)))
while ( (!is.numeric(iden[8])) || (round(iden[8],0) != iden[8]) || (iden[8] < 0) ) {
	cat("Digite o num.de pontos a ser identificado (0=nenhum) e <enter> para continuar\n")
	out <- readline()
	iden[8]<-as.numeric(out)
}
if(iden[8]>0) {identify(m,abs(r),n=iden[8],labels=nome)}

par(mfrow=c(1,1))

list(ResQuantil=rq,ResCompDesv=td,ResAnscombe=ra,ResPearsonStd=ts,ResWilliams=rw,Di=di,Ci=ci,Dmax=dmax,h=h)
}
