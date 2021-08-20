library(readxl)
dados <- read_excel("C:/Users/criso/Desktop/alura/Regressão Linear Simples com R/regressaolinear_r-master/dados.xlsx", 
    col_types = c("numeric", "numeric", "skip"))
View(dados)

#Inserindo um gráfico de dispersão
plot(dados$area,dados$preco) #primeira variável é a que influencia a outra
plot(dados$area,dados$preco, main="Diagrama de Dispersão") #inserindo o título
plot(dados$area,dados$preco, main="Diagrama de Dispersão", xlab= "Área",ylab = "Preço") #inserindo o nome dos eixos
plot(dados$area,dados$preco, main="Diagrama de Dispersão", xlab= "Área",ylab = "Preço", pch=19) #alterando o formato dos pontos no gráfico
plot(dados$area,dados$preco, main="Diagrama de Dispersão", xlab= "Área",ylab = "Preço", pch=19, col="blue")

#Identificando a correlação entre os dados
cor(dados$area,dados$preco) #Coeficiente de correlação de Pearson
cor.test(dados$area,dados$preco) #Teste de correlação

#Plotando um boxplot
boxplot(dados)

#Instalando o pacote "car" em tools
library(car) #Para carregar o pacote
Boxplot(dados$preco) #encontrando a casa que tem o valor fora do gráfico do do boxplot
dados$preco[79] #encontrando o valor da casa 79

which(dados$preco > quantile(dados$preco,0.75)) #saber quais os índices com os maiores preços

#Modelagem de dados

#carregar novamente os dados, dessa vez todas as colunas
library(readxl)
dados <- read_excel("C:/Users/criso/Desktop/alura/Regressão Linear Simples com R/regressaolinear_r-master/dados.xlsx")
View(dados)

#encontrando os coeficientes
modl=lm(preco~area, data=dados) #Equação: preco=b0+b1*area

#Encontrando o valor para preço com 70m2, substituindo os coeficientes
preco_70=502347+7851*70
preco_70

#pode ser feito também assim:
modl1$coefficients[[1]]+modl2$coefficients[[2]]*70

#Inserindo a reta no gráfico
plot(dados$area,dados$preco, main="Diagrama e Reta") #plotando o gráfico de dispersão
abline(modl, col="red") #adicionando a reta e mudando a cor

#Comprovando o modelo
names(summary(modl)) #informações presentes no resumo
summary(modl)$r.squared #valor do r2

#Validação do modelo
plot(modl$residuals)
identify(modl$residuals,n=2)
dados_59=dados[-59,] #retirando o residual da casa 59
dados_5982=dados[c(-59,-82),] #retirando os residuals das casas 59 e 82

#teste do Durbin Watson 
#instalar o pacote lmtest
library(lmtest)
dwtest(mod1)
plot(mod1$fitted.values,mod1$residuals)

#Avaliar: para p-value < 0,05, rejeitar hipótese nula

#teste de Breusch Pagan
bptest(mod1)

#Avaliar a normalidade

#Visualmente, avaliar a normalidade dos erros pelo gráfico qqplot ou gráfico de distribuição
plot(mod1,2)

#teste de Shapiro Wilks
#teste para verificar a normalidade dos dados
shapiro.test(mod1$residuals)

##análise realizada para ajustar um modelo linear simples é baseado em várias suposições sobre o modelo
par(mfrow = c(2,2))
#independência dos dados
plot(modelo$residuals,
main = "Residuos",
ylab = "")
#homoscedasticidade:
plot(modelo$fitted.values,modelo$residuals,
main = "Resíduos contra notas \n preditas dos alunos \n",
xlab = "Notas preditas dos alunos",
ylab = "Resíduos")
#normalidade dos resíduos:
plot(modelo,2)

#verificar
#* Se o gráfico dos resíduos não apresenta tendência ou relação há indícios de independência dos resíduos.
#* Se o gráfico dos resíduos contra os valores ajustados  não apresenta tendência ou relação há indícios de homoscedasticidade dos resíduos.
#* Se no gráfico Q-Q plot os pontos seguem a linha traçada há indícios de normalidade dos resíduos.

#Previsão 
dados_novos=data.frame(area=c(60,70))
> predict(mod1, newdata = dados_novos)
predict(mod1, newdata = dados_novos,interval="prediction") #intervalo de predição
predict(mod1, newdata = dados_novos,interval="confidence") #intervalo de confiança







