## Pacotes -----------------------------------------------------------------

# Abrir base .dta
library(haven)

# Estatistica ponderada
library(modi)

# Gini ponderado 
library(acid)

# Coeficiente de concentração
library(DescTools)

# Tabela resultados
library(xlsx)

# Pseudo-Gini
# https://search.r-project.org/CRAN/refmans/GiniWegNeg/html/PGini_RSV.html
setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1XkWCFJTXwf7XauXhi5Y-ws3vQ-Ao49xK/DRIVE DO MADE /DIRPF + POF")

## Imposto -----------------------------------------------------------------
base <- read_dta("/Volumes/GoogleDrive/.shortcut-targets-by-id/1XkWCFJTXwf7XauXhi5Y-ws3vQ-Ao49xK/DRIVE DO MADE /DIRPF + POF/base final.dta")

base <- read_dta("base final.dta")

### Calcular base de calculo a partir do IR atual ###

faixa1=22847.76
faixa2=33919.80
faixa3=45012.60
faixa4=55976.16 

aliquota1=0.075
aliquota2=0.15
aliquota3=0.225
aliquota4=0.275   

deduzir1=1713.58 
deduzir2=4257.57
deduzir3=7633.51
deduzir4=10432.32

imposto1=0
imposto2=(faixa2-faixa1)*aliquota1
imposto3=((faixa3-faixa2)*aliquota2) + imposto2  
imposto4=((faixa4-faixa3)*aliquota3) + imposto3 

# Estimar base de cálculo

base$base_calculo_est <- NA

base$base_calculo_est[base$tribut_RT>(imposto1/12)] <- (base$tribut_RT[base$tribut_RT>(imposto1/12)]+(deduzir1/12))/aliquota1

base$base_calculo_est[base$tribut_RT>(imposto2/12)] <- (base$tribut_RT[base$tribut_RT>(imposto2/12)]+(deduzir2/12))/aliquota2

base$base_calculo_est[base$tribut_RT>(imposto3/12)] <- (base$tribut_RT[base$tribut_RT>(imposto3/12)]+(deduzir3/12))/aliquota3

base$base_calculo_est[base$tribut_RT>(imposto4/12)] <- (base$tribut_RT[base$tribut_RT>(imposto4/12)]+(deduzir4/12))/aliquota4

base$base_calculo_est[is.na(base$base_calculo_est)] <- 0  

# Cálculo cenario atual

base$IRPF <- NA

base$IRPF[base$base_calculo_est*12 <= faixa1] <- 0 

base$IRPF[base$base_calculo_est*12 > faixa1 & 
                           base$base_calculo_est*12 <= faixa2] <-
  (base$base_calculo_est[base$base_calculo_est*12 > faixa1 & 
                           base$base_calculo_est*12 <= faixa2]-(faixa1/12))*aliquota1

base$IRPF[base$base_calculo_est*12 > faixa2 & 
                           base$base_calculo_est*12 <= faixa3] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa2 & 
                           base$base_calculo_est*12 <= faixa3]-(faixa2/12))*aliquota2

base$IRPF[base$base_calculo_est*12 > faixa3 & 
            base$base_calculo_est*12 <= faixa4] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa3 & 
                           base$base_calculo_est*12 <= faixa4]-(faixa3/12))*aliquota3

base$IRPF[base$base_calculo_est*12 > faixa4] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  ((faixa4/12)-(faixa3/12))*aliquota3 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa4]-(faixa4/12))*aliquota4

base$IRPF[is.na(base$IRPF)] <- 0

# Gerar renda disponivel pós imposto

base$IRPF_pago <- base$IRPF + base$tribut_RTE_trab + base$tribut_RTE_cap

base$renda_disponivel = base$renda_irpfepof - base$IRPF_pago



### Proposta 1 - PL Danilo Fortes ###

faixa1=5200*12
faixa2=6084*12
faixa3=7608*12
faixa4=9116.12*12 

aliquota1=0.075
aliquota2=0.15
aliquota3=0.225  
aliquota4=0.275   

deduzir1=faixa1*(aliquota1-0)
deduzir2=(faixa2-faixa1)*(aliquota2-aliquota1)+faixa1*(aliquota2-0)
deduzir3=(faixa3-faixa2)*(aliquota3-aliquota2)+(faixa2-faixa1)*(aliquota3-aliquota1)+faixa1*(aliquota3-0)
deduzir4=(faixa4-faixa3)*(aliquota4-aliquota3)+(faixa3-faixa2)*(aliquota4-aliquota2)+(faixa2-faixa1)*(aliquota4-aliquota1)+faixa1*(aliquota4-0)

imposto1=0
imposto2=(faixa2-faixa1)*aliquota1
imposto3=((faixa3-faixa2)*aliquota2) + imposto2  
imposto4=((faixa4-faixa3)*aliquota3) + imposto3 

# Cálculo do imposto segundo Proposta 1

base$IRPF_P1 <- NA

base$IRPF_P1[base$base_calculo_est*12 <= faixa1] <- 0 

base$IRPF_P1[base$base_calculo_est*12 > faixa1 & 
            base$base_calculo_est*12 <= faixa2] <-
  (base$base_calculo_est[base$base_calculo_est*12 > faixa1 & 
                           base$base_calculo_est*12 <= faixa2]-(faixa1/12))*aliquota1

base$IRPF_P1[base$base_calculo_est*12 > faixa2 & 
            base$base_calculo_est*12 <= faixa3] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa2 & 
                           base$base_calculo_est*12 <= faixa3]-(faixa2/12))*aliquota2

base$IRPF_P1[base$base_calculo_est*12 > faixa3 & 
            base$base_calculo_est*12 <= faixa4] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa3 & 
                           base$base_calculo_est*12 <= faixa4]-(faixa3/12))*aliquota3

base$IRPF_P1[base$base_calculo_est*12 > faixa4] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  ((faixa4/12)-(faixa3/12))*aliquota3 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa4]-(faixa4/12))*aliquota4

base$IRPF_P1[is.na(base$IRPF_P1)] <- 0

# Gerar renda disponivel

base$IRPF_P1_pago <- base$IRPF_P1 + base$tribut_RTE_trab + base$tribut_RTE_cap

base$renda_disponivel_P1 = base$renda_irpfepof - base$IRPF_P1_pago

# Impacto fiscal ----------------------------------------------------------

# Calcular perda arrecadação por mês (R$ Bilhões)

arrecadacao_IRPF <- sum(base$IRPF_pago*base$peso_final)/10^9

arrecadacao_P1 <- sum(base$IRPF_P1_pago*base$peso_final)/10^9



### Proposta 2 - correção pela inflação ###

faixa1=33280.32
faixa2=49407.99
faixa3=65565.89
faixa4=81535.55

aliquota1=0.075
aliquota2=0.15
aliquota3=0.225  
aliquota4=0.275   

deduzir1=faixa1*(aliquota1-0)
deduzir2=(faixa2-faixa1)*(aliquota2-aliquota1)+faixa1*(aliquota2-0)
deduzir3=(faixa3-faixa2)*(aliquota3-aliquota2)+(faixa2-faixa1)*(aliquota3-aliquota1)+faixa1*(aliquota3-0)
deduzir4=(faixa4-faixa3)*(aliquota4-aliquota3)+(faixa3-faixa2)*(aliquota4-aliquota2)+(faixa2-faixa1)*(aliquota4-aliquota1)+faixa1*(aliquota4-0)

imposto1=0
imposto2=(faixa2-faixa1)*aliquota1
imposto3=((faixa3-faixa2)*aliquota2) + imposto2  
imposto4=((faixa4-faixa3)*aliquota3) + imposto3 

# Cálculo do imposto segundo Proposta 2 

base$IRPF_P2 <- NA

base$IRPF_P2[base$base_calculo_est*12 <= faixa1] <- 0 

base$IRPF_P2[base$base_calculo_est*12 > faixa1 & 
               base$base_calculo_est*12 <= faixa2] <-
  (base$base_calculo_est[base$base_calculo_est*12 > faixa1 & 
                           base$base_calculo_est*12 <= faixa2]-(faixa1/12))*aliquota1

base$IRPF_P2[base$base_calculo_est*12 > faixa2 & 
               base$base_calculo_est*12 <= faixa3] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa2 & 
                           base$base_calculo_est*12 <= faixa3]-(faixa2/12))*aliquota2

base$IRPF_P2[base$base_calculo_est*12 > faixa3 & 
               base$base_calculo_est*12 <= faixa4] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa3 & 
                           base$base_calculo_est*12 <= faixa4]-(faixa3/12))*aliquota3

base$IRPF_P2[base$base_calculo_est*12 > faixa4] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  ((faixa4/12)-(faixa3/12))*aliquota3 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa4]-(faixa4/12))*aliquota4


base$IRPF_P2[is.na(base$IRPF_P3)] <- 0

# Gerar renda disponivel

base$IRPF_P2_pago <- base$IRPF_P2 + base$tribut_RTE_trab + base$tribut_RTE_cap

base$renda_disponivel_P2 <- base$renda_irpfepof - base$IRPF_P2_pago

# Calcular arrecadação Proposta 2 (R$ Bilhões)

arrecadacao_P2 <- sum(base$IRPF_P2_pago*base$peso_final)/10^9




### Proposta 3 - Aumento das faixas por *1.386569, isenção até 2 SM ###

faixa0=22847.76*1.386569
faixa1=22847.76*1.109255
faixa2=33919.80*1
faixa3=45012.60*1
faixa4=55976.16*1


aliquota1=0.075
aliquota2=0.15
aliquota3=0.225  
aliquota4=0.275   

deduzir1=faixa1*(aliquota1-0)
deduzir2=(faixa2-faixa1)*(aliquota2-aliquota1)+faixa1*(aliquota2-0)
deduzir3=(faixa3-faixa2)*(aliquota3-aliquota2)+(faixa2-faixa1)*(aliquota3-aliquota1)+faixa1*(aliquota3-0)
deduzir4=(faixa4-faixa3)*(aliquota4-aliquota3)+(faixa3-faixa2)*(aliquota4-aliquota2)+(faixa2-faixa1)*(aliquota4-aliquota1)+faixa1*(aliquota4-0)

imposto1=0
imposto2=(faixa2-faixa1)*aliquota1
imposto3=((faixa3-faixa2)*aliquota2) + imposto2  
imposto4=((faixa4-faixa3)*aliquota3) + imposto3 

# Cálculo do imposto segundo Proposta 3 

base$IRPF_P3 <- NA

base$IRPF_P3[base$base_calculo_est*12 <= faixa0] <- 0 

base$IRPF_P3[base$base_calculo_est*12 > faixa1 & 
               base$base_calculo_est*12 <= faixa2] <-
  (base$base_calculo_est[base$base_calculo_est*12 > faixa1 & 
                           base$base_calculo_est*12 <= faixa2]-(faixa1/12))*aliquota1

base$IRPF_P3[base$base_calculo_est*12 > faixa2 & 
               base$base_calculo_est*12 <= faixa3] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa2 & 
                           base$base_calculo_est*12 <= faixa3]-(faixa2/12))*aliquota2

base$IRPF_P3[base$base_calculo_est*12 > faixa3 & 
               base$base_calculo_est*12 <= faixa4] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa3 & 
                           base$base_calculo_est*12 <= faixa4]-(faixa3/12))*aliquota3

base$IRPF_P3[base$base_calculo_est*12 > faixa4] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  ((faixa4/12)-(faixa3/12))*aliquota3 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa4]-(faixa4/12))*aliquota4


base$IRPF_P3[is.na(base$IRPF_P3)] <- 0

# Gerar renda disponivel

base$IRPF_P3_pago <- base$IRPF_P3 + base$tribut_RTE_trab + base$tribut_RTE_cap

base$renda_disponivel_P3 <- base$renda_irpfepof - base$IRPF_P3_pago

# Calcular arrecadação por mês Proposta 3 (R$ Bilhões)

arrecadacao_P3 <- sum(base$IRPF_P3_pago*base$peso_final)/10^9


##### Adicionar medidas de compensação #####


### Compensação: Proposta 1 + 35/15 ###

faixa1=5200*12
faixa2=6084*12
faixa3=7608*12
faixa4=9116.12*12 
faixa5=322295

aliquota1=0.075
aliquota2=0.15
aliquota3=0.225  
aliquota4=0.275   
aliquota5=0.35

deduzir1=faixa1*(aliquota1-0)
deduzir2=(faixa2-faixa1)*(aliquota2-aliquota1)+faixa1*(aliquota2-0)
deduzir3=(faixa3-faixa2)*(aliquota3-aliquota2)+(faixa2-faixa1)*(aliquota3-aliquota1)+faixa1*(aliquota3-0)
deduzir4=(faixa4-faixa3)*(aliquota4-aliquota3)+(faixa3-faixa2)*(aliquota4-aliquota2)+(faixa2-faixa1)*(aliquota4-aliquota1)+faixa1*(aliquota4-0)
deduzir5=(faixa5-faixa4)*(aliquota5-aliquota4)+(faixa4-faixa3)*(aliquota5-aliquota3)+(faixa3-faixa2)*(aliquota5-aliquota2)+(faixa2-faixa1)*(aliquota5-aliquota1)+faixa1*(aliquota5-0)

imposto1=0
imposto2=(faixa2-faixa1)*aliquota1
imposto3=((faixa3-faixa2)*aliquota2) + imposto2  
imposto4=((faixa4-faixa3)*aliquota3) + imposto3 
imposto5=((faixa5-faixa4)*aliquota4) + imposto4

# Cálculo do imposto segundo Proposta 1 + 35/15 

base$IRPF_P1_35 <- NA

base$IRPF_P1_35[base$base_calculo_est*12 <= faixa1] <- 0 

base$IRPF_P1_35[base$base_calculo_est*12 > faixa1 & 
                   base$base_calculo_est*12 <= faixa2] <-
  (base$base_calculo_est[base$base_calculo_est*12 > faixa1 & 
                           base$base_calculo_est*12 <= faixa2]-(faixa1/12))*aliquota1

base$IRPF_P1_35[base$base_calculo_est*12 > faixa2 & 
                   base$base_calculo_est*12 <= faixa3] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa2 & 
                           base$base_calculo_est*12 <= faixa3]-(faixa2/12))*aliquota2

base$IRPF_P1_35[base$base_calculo_est*12 > faixa3 & 
                   base$base_calculo_est*12 <= faixa4] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa3 & 
                           base$base_calculo_est*12 <= faixa4]-(faixa3/12))*aliquota3

base$IRPF_P1_35[base$base_calculo_est*12 > faixa4 & 
               base$base_calculo_est*12 <= faixa5] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  ((faixa4/12)-(faixa3/12))*aliquota3 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa4 & 
                           base$base_calculo_est*12 <= faixa5]-(faixa4/12))*aliquota4

base$IRPF_P1_35[base$base_calculo_est*12 > faixa5] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  ((faixa4/12)-(faixa3/12))*aliquota3 +
  ((faixa5/12)-(faixa4/12))*aliquota4 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa5]-(faixa5/12))*aliquota5

base$IRPF_P1_35[is.na(base$IRPF_P1_35)] <- 0

# Gerar renda disponivel

base$IRPF_P1_35_pago <- base$IRPF_P1_35 + base$tribut_RTE_trab + base$tribut_RTE_cap + .15*base$lucrosdividendos

base$renda_disponivel_P1_35 = base$renda_irpfepof - base$IRPF_P1_35_pago

# Calcular arrecadação P1_35 por mês (R$ Bilhões)

arrecadacao_P1_35 <- sum(base$IRPF_P1_35_pago*base$peso_final)/10^9

### Compensação: Proposta 2 + 35/15 ###

faixa1=33280.32
faixa2=49407.99
faixa3=65565.89
faixa4=81535.55
faixa5=322295

aliquota1=0.075
aliquota2=0.15
aliquota3=0.225  
aliquota4=0.275   
aliquota5=0.35

deduzir1=faixa1*(aliquota1-0)
deduzir2=(faixa2-faixa1)*(aliquota2-aliquota1)+faixa1*(aliquota2-0)
deduzir3=(faixa3-faixa2)*(aliquota3-aliquota2)+(faixa2-faixa1)*(aliquota3-aliquota1)+faixa1*(aliquota3-0)
deduzir4=(faixa4-faixa3)*(aliquota4-aliquota3)+(faixa3-faixa2)*(aliquota4-aliquota2)+(faixa2-faixa1)*(aliquota4-aliquota1)+faixa1*(aliquota4-0)
deduzir5=(faixa5-faixa4)*(aliquota5-aliquota4)+(faixa4-faixa3)*(aliquota5-aliquota3)+(faixa3-faixa2)*(aliquota5-aliquota2)+(faixa2-faixa1)*(aliquota5-aliquota1)+faixa1*(aliquota5-0)

imposto1=0
imposto2=(faixa2-faixa1)*aliquota1
imposto3=((faixa3-faixa2)*aliquota2) + imposto2  
imposto4=((faixa4-faixa3)*aliquota3) + imposto3 
imposto5=((faixa5-faixa4)*aliquota4) + imposto4

# Cálculo do imposto segundo Proposta 2 + 35/15

base$IRPF_P2_35 <- NA

base$IRPF_P2_35[base$base_calculo_est*12 <= faixa1] <- 0 

base$IRPF_P2_35[base$base_calculo_est*12 > faixa1 & 
               base$base_calculo_est*12 <= faixa2] <-
  (base$base_calculo_est[base$base_calculo_est*12 > faixa1 & 
                           base$base_calculo_est*12 <= faixa2]-(faixa1/12))*aliquota1

base$IRPF_P2_35[base$base_calculo_est*12 > faixa2 & 
               base$base_calculo_est*12 <= faixa3] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa2 & 
                           base$base_calculo_est*12 <= faixa3]-(faixa2/12))*aliquota2

base$IRPF_P2_35[base$base_calculo_est*12 > faixa3 & 
               base$base_calculo_est*12 <= faixa4] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa3 & 
                           base$base_calculo_est*12 <= faixa4]-(faixa3/12))*aliquota3

base$IRPF_P2_35[base$base_calculo_est*12 > faixa4 & 
               base$base_calculo_est*12 <= faixa5] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  ((faixa4/12)-(faixa3/12))*aliquota3 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa4 & 
                           base$base_calculo_est*12 <= faixa5]-(faixa4/12))*aliquota4

base$IRPF_P2_35[base$base_calculo_est*12 > faixa5] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  ((faixa4/12)-(faixa3/12))*aliquota3 +
  ((faixa5/12)-(faixa4/12))*aliquota4 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa5]-(faixa5/12))*aliquota5

base$IRPF_P2_35[is.na(base$IRPF_P2_35)] <- 0

# Gerar renda disponivel

base$IRPF_P2_35_pago <- base$IRPF_P2_35 + base$tribut_RTE_trab + base$tribut_RTE_cap + .15*base$lucrosdividendos

base$renda_disponivel_P2_35 <- base$renda_irpfepof - base$IRPF_P2_35_pago

# Calcular arrecadação Proposta 2 + 35/15 por mês (R$ Bilhões)

arrecadacao_P2_35 <- sum(base$IRPF_P2_35_pago*base$peso_final)/10^9


### Compensação: Proposta 3 + 35/15 ###

faixa0=22847.76*1.386569
faixa1=22847.76*1.109255
faixa2=33919.80*1
faixa3=45012.60*1
faixa4=55976.16*1
faixa5=322295

aliquota1=0.075
aliquota2=0.15
aliquota3=0.225  
aliquota4=0.275   
aliquota5=0.35

deduzir1=faixa1*(aliquota1-0)
deduzir2=(faixa2-faixa1)*(aliquota2-aliquota1)+faixa1*(aliquota2-0)
deduzir3=(faixa3-faixa2)*(aliquota3-aliquota2)+(faixa2-faixa1)*(aliquota3-aliquota1)+faixa1*(aliquota3-0)
deduzir4=(faixa4-faixa3)*(aliquota4-aliquota3)+(faixa3-faixa2)*(aliquota4-aliquota2)+(faixa2-faixa1)*(aliquota4-aliquota1)+faixa1*(aliquota4-0)
deduzir5=(faixa5-faixa4)*(aliquota5-aliquota4)+(faixa4-faixa3)*(aliquota5-aliquota3)+(faixa3-faixa2)*(aliquota5-aliquota2)+(faixa2-faixa1)*(aliquota5-aliquota1)+faixa1*(aliquota5-0)

imposto1=0
imposto2=(faixa2-faixa1)*aliquota1
imposto3=((faixa3-faixa2)*aliquota2) + imposto2  
imposto4=((faixa4-faixa3)*aliquota3) + imposto3 
imposto5=((faixa5-faixa4)*aliquota4) + imposto4

# Cálculo do imposto segundo Proposta 2 + 35/15

base$IRPF_P3_35 <- NA

base$IRPF_P3_35[base$base_calculo_est*12 <= faixa0] <- 0 

base$IRPF_P3_35[base$base_calculo_est*12 > faixa1 & 
               base$base_calculo_est*12 <= faixa2] <-
  (base$base_calculo_est[base$base_calculo_est*12 > faixa1 & 
                           base$base_calculo_est*12 <= faixa2]-(faixa1/12))*aliquota1

base$IRPF_P3_35[base$base_calculo_est*12 > faixa2 & 
               base$base_calculo_est*12 <= faixa3] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa2 & 
                           base$base_calculo_est*12 <= faixa3]-(faixa2/12))*aliquota2

base$IRPF_P3_35[base$base_calculo_est*12 > faixa3 & 
               base$base_calculo_est*12 <= faixa4] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa3 & 
                           base$base_calculo_est*12 <= faixa4]-(faixa3/12))*aliquota3

base$IRPF_P3_35[base$base_calculo_est*12 > faixa4 & 
               base$base_calculo_est*12 <= faixa5] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  ((faixa4/12)-(faixa3/12))*aliquota3 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa4 & 
                           base$base_calculo_est*12 <= faixa5]-(faixa4/12))*aliquota4

base$IRPF_P3_35[base$base_calculo_est*12 > faixa5] <-
  ((faixa2/12)-(faixa1/12))*aliquota1 +
  ((faixa3/12)-(faixa2/12))*aliquota2 +
  ((faixa4/12)-(faixa3/12))*aliquota3 +
  ((faixa5/12)-(faixa4/12))*aliquota4 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa5]-(faixa5/12))*aliquota5

base$IRPF_P3_35[is.na(base$IRPF_P3)] <- 0

# Gerar renda disponivel

base$IRPF_P3_35_pago <- base$IRPF_P3_35 + base$tribut_RTE_trab + base$tribut_RTE_cap + .15*base$lucrosdividendos

base$renda_disponivel_P3_35 <- base$renda_irpfepof - base$IRPF_P3_35_pago

# Calcular arrecadação Proposta 3 + 35/15 por mês (R$ Bilhões)

arrecadacao_P3_35 <- sum(base$IRPF_P3_35_pago*base$peso_final)/10^9

# Simulação alíquota ------------------------------------------------------

### Alíquotas para 1% ###

aliquotas <- seq(.35, 1, by = .05)

arrecadacao <- data.frame()

for (q in aliquotas) {
  
  faixa1=5200*12
  faixa2=6084*12
  faixa3=7608*12
  faixa4=9116.12*12 
  faixa5=322295
  
  aliquota1=0.075
  aliquota2=0.15
  aliquota3=0.225  
  aliquota4=0.275   
  aliquota5=q
  
  deduzir1=faixa1*(aliquota1-0)
  deduzir2=(faixa2-faixa1)*(aliquota2-aliquota1)+faixa1*(aliquota2-0)
  deduzir3=(faixa3-faixa2)*(aliquota3-aliquota2)+(faixa2-faixa1)*(aliquota3-aliquota1)+faixa1*(aliquota3-0)
  deduzir4=(faixa4-faixa3)*(aliquota4-aliquota3)+(faixa3-faixa2)*(aliquota4-aliquota2)+(faixa2-faixa1)*(aliquota4-aliquota1)+faixa1*(aliquota4-0)
  deduzir5=(faixa5-faixa4)*(aliquota5-aliquota4)+(faixa4-faixa3)*(aliquota5-aliquota3)+(faixa3-faixa2)*(aliquota5-aliquota2)+(faixa2-faixa1)*(aliquota5-aliquota1)+faixa1*(aliquota5-0)
  
  imposto1=0
  imposto2=(faixa2-faixa1)*aliquota1
  imposto3=((faixa3-faixa2)*aliquota2) + imposto2  
  imposto4=((faixa4-faixa3)*aliquota3) + imposto3 
  imposto5=((faixa5-faixa4)*aliquota4) + imposto4
  
  # Cálculo do imposto
  
  base$IRPF_Ali <- NA
  
  base$IRPF_Ali[base$base_calculo_est*12 <= faixa1] <- 0 
  
  base$IRPF_Ali[base$base_calculo_est*12 > faixa1 & 
                  base$base_calculo_est*12 <= faixa2] <-
    (base$base_calculo_est[base$base_calculo_est*12 > faixa1 & 
                             base$base_calculo_est*12 <= faixa2]-(faixa1/12))*aliquota1
  
  base$IRPF_Ali[base$base_calculo_est*12 > faixa2 & 
                  base$base_calculo_est*12 <= faixa3] <-
    ((faixa2/12)-(faixa1/12))*aliquota1 +
    (base$base_calculo_est[base$base_calculo_est*12 > faixa2 & 
                             base$base_calculo_est*12 <= faixa3]-(faixa2/12))*aliquota2
  
  base$IRPF_Ali[base$base_calculo_est*12 > faixa3 & 
                  base$base_calculo_est*12 <= faixa4] <-
    ((faixa2/12)-(faixa1/12))*aliquota1 +
    ((faixa3/12)-(faixa2/12))*aliquota2 +
    (base$base_calculo_est[base$base_calculo_est*12 > faixa3 & 
                             base$base_calculo_est*12 <= faixa4]-(faixa3/12))*aliquota3
  
  base$IRPF_Ali[base$base_calculo_est*12 > faixa4 & 
                  base$base_calculo_est*12 <= faixa5] <-
    ((faixa2/12)-(faixa1/12))*aliquota1 +
    ((faixa3/12)-(faixa2/12))*aliquota2 +
    ((faixa4/12)-(faixa3/12))*aliquota3 +
    (base$base_calculo_est[base$base_calculo_est*12 > faixa4 & 
                             base$base_calculo_est*12 <= faixa5]-(faixa4/12))*aliquota4
  
  base$IRPF_Ali[base$base_calculo_est*12 > faixa5] <-
    ((faixa2/12)-(faixa1/12))*aliquota1 +
    ((faixa3/12)-(faixa2/12))*aliquota2 +
    ((faixa4/12)-(faixa3/12))*aliquota3 +
    ((faixa5/12)-(faixa4/12))*aliquota4 +
    (base$base_calculo_est[base$base_calculo_est*12 > faixa5]-(faixa5/12))*aliquota5
  
  base$IRPF_Ali[is.na(base$IRPF_Ali)] <- 0
  
  # Gerar renda disponivel
  
  base$IRPF_Ali_pago <- base$IRPF_Ali + base$tribut_RTE_trab + base$tribut_RTE_cap
  
  # Calcular arrecadação por mês (R$ Bilhões)
  
  arrecadacao_ali <- sum(base$IRPF_Ali_pago*base$peso_final)/10^9
  
  arrecadacao <- rbind(arrecadacao,
                       data.frame("Aliquota" = paste0(q*100, "%"),
                                  "Impacto Fiscal" = 12*(arrecadacao_ali - arrecadacao_IRPF)))
  
}

# Estatísticas distributivas ----------------------------------------------

### Coeficiente de concentração ###

base <- base[order(base$renda_irpfepof),]

base$ordem <- 1:nrow(base)

base$prop_pop <- cumsum(base$peso_final/sum(base$peso_final))

base$cc_irpf <- cumsum(base$IRPF_pago*base$peso_final/sum(base$IRPF_pago*base$peso_final))

#base$cc_irpf_danilo <- cumsum(base$IRPF_danilo_pago*base$peso_final/sum(base$IRPF_danilo_pago*base$peso_final))

base$cc_irpf_p1 <- cumsum(base$IRPF_P1_pago*base$peso_final/sum(base$IRPF_P1_pago*base$peso_final))

base$cc_irpf_p2 <- cumsum(base$IRPF_P2_pago*base$peso_final/sum(base$IRPF_P2_pago*base$peso_final))

base$cc_irpf_p3 <- cumsum(base$IRPF_P3_pago*base$peso_final/sum(base$IRPF_P3_pago*base$peso_final))

base$cc_irpf_p1_35 <- cumsum(base$IRPF_P1_35_pago*base$peso_final/sum(base$IRPF_P1_35_pago*base$peso_final))

base$cc_irpf_p2_35 <- cumsum(base$IRPF_P2_35_pago*base$peso_final/sum(base$IRPF_P2_35_pago*base$peso_final))

base$cc_irpf_p3_35 <- cumsum(base$IRPF_P3_35_pago*base$peso_final/sum(base$IRPF_P3_35_pago*base$peso_final))


base$cc_dividendo <- cumsum(base$lucrosdividendos*base$peso_final/sum(base$lucrosdividendos*base$peso_final))

base$gini <- cumsum(base$renda_irpfepof*base$peso_final/sum(base$renda_irpfepof*base$peso_final))

# Gráficos

png("CC_PL2140.png", width = 867.072, height = 501.504, res = 120)

plot(y = base$gini,
     x = base$prop_pop,
     main ='Propostas sem Medidas de Compensação',
     type = "l",
     xlab = "Pop. ordenada por renda",
     ylab = "")

lines(y = seq(0,1,length = nrow(base)),
      x = seq(0,1,length = nrow(base)),
      lty = 2)

lines(y = base$cc_irpf,
      x = base$prop_pop, 
      col = "#45ff66")

lines(y = base$cc_irpf_p1,
      x = base$prop_pop,
      col = "#EB52ff")

lines(y = base$cc_irpf_p2,
      x = base$prop_pop,
      col = "#3366ff")

lines(y = base$cc_irpf_p3,
      x = base$prop_pop,
      col = "#feff41")

legend("topleft",
       legend = c("Renda", "IRPF", "PL 2.140/22 ","Correção Pela Inflação","Isenção até 2SM"),
       col = c("black", 
               "#45ff66",
               "#EB52ff",
               "#3366ff",
               "#feff41"),
       lty = 1,
       bty = "n")

dev.off()

png("CC_PL2140_35.png", width = 867.072, height = 501.504, res = 120)

plot(y = base$gini,
     x = base$prop_pop,
     main = 'Propostas com Medidas de Compensação',
     type = "l",
     xlab = "Pop. ordenada por renda",
     ylab = "")

lines(y = seq(0,1,length = nrow(base)),
      x = seq(0,1,length = nrow(base)),
      lty = 2)

lines(y = base$cc_irpf,
      x = base$prop_pop, 
      col = "#45ff66")

lines(y = base$cc_irpf_p1_35,
      x = base$prop_pop,
      col = "#EB52ff")

lines(y = base$cc_irpf_p2_35,
      x = base$prop_pop,
      col = "#3366ff")

lines(y = base$cc_irpf_p3_35,
      x = base$prop_pop,
      col = "#feff41")

legend("topleft",
       legend = c("Renda", "IRPF", "PL 2.140/22","Correção Pela Inflação","Isenção até 2SM"),
       col = c("black", 
                      "#45ff66",
                      "#EB52ff",
                      "#3366ff",
                      "#feff41"),
                      lty = 1,
       bty = "n")

dev.off()

# Coeficiente de concentração

# Gini
gini <- 2*(0.5 - AUC(x = base$prop_pop, y = base$gini))

# IRPF
cc_irpf <- 2*(0.5 - AUC(x = base$prop_pop, y = base$cc_irpf))

# IRPF P1
cc_irpf_p1 <- 2*(0.5 - AUC(x = base$prop_pop, y = base$cc_irpf_p1))

# IRPF P2
cc_irpf_p2 <- 2*(0.5 - AUC(x = base$prop_pop, y = base$cc_irpf_p2))

# IRPF P3
cc_irpf_p3 <- 2*(0.5 - AUC(x = base$prop_pop, y = base$cc_irpf_p3))


# IRPF P1 35
cc_irpf_p1_35 <- 2*(0.5 - AUC(x = base$prop_pop, y = base$cc_irpf_p1_35))

# IRPF P2 35
cc_irpf_p2_35 <- 2*(0.5 - AUC(x = base$prop_pop, y = base$cc_irpf_p2_35))

# IRPF P3 35
cc_irpf_p3_35 <- 2*(0.5 - AUC(x = base$prop_pop, y = base$cc_irpf_p3_35))

### Gini ###

# Gini - IRPF atual

gini_IRPF <- weighted.gini(x = base$renda_disponivel,
              w = base$peso_final)$Gini[1][1]

# Gini - Proposta 1

gini_P1 <- weighted.gini(x = base$renda_disponivel_P1,
              w = base$peso_final)$Gini[1][1]

# Gini - Proposta 2

gini_P2 <- weighted.gini(x = base$renda_disponivel_P2,
              w = base$peso_final)$Gini[1][1]

# Gini - Proposta 3

gini_P3 <- weighted.gini(x = base$renda_disponivel_P3,
              w = base$peso_final)$Gini[1][1]

# Gini - Proposta 1 + 35/12

gini_P1_35 <- weighted.gini(x = base$renda_disponivel_P1_35,
              w = base$peso_final)$Gini[1][1]

# Gini - Proposta 2 + 35/12

gini_P2_35 <- weighted.gini(x = base$renda_disponivel_P2_35,
              w = base$peso_final)$Gini[1][1]

# Gini - Proposta 3 + 35/12

gini_P3_35 <- weighted.gini(x = base$renda_disponivel_P3_35,
              w = base$peso_final)$Gini[1][1]

gini_arrecadao <- data.frame('Dado'=c('Arredação','Gini'), 'IRPF'=c(arrecadacao_IRPF,gini_IRPF),'P1'=c(arrecadacao_P1,gini_P1),'P2'=c(arrecadacao_P2,gini_P2),'P3'=c(arrecadacao_P3,gini_P3),'P1_35'=c(arrecadacao_P1_35,gini_P1_35),'P2_35'=c(arrecadacao_P2_35, gini_P2_35),'P3_35'=c(arrecadacao_P3_35, gini_P3_35))

### Participação 1% e 10$ do topo na renda total ###

rendas <- c("renda_irpfepof", "renda_disponivel", "renda_disponivel_P1", 
            "renda_disponivel_P2", "renda_disponivel_P3", "renda_disponivel_P1_35","renda_disponivel_P2_35","renda_disponivel_P3_35")


# Participação dos 10% 

dezpc <- data.frame()

for (r in rendas) {
  
  dezpart <- (sum(((base[[r]])*base$peso_final)[base$renda_irpfepof*12 >= 54667.55]))/sum(base[[r]]*base$peso_final)
  
  dezpc <- rbind(dezpc,
                        data.frame("Renda" = r, "Valor" = dezpart))
  
}

#Participação do 1%

umpc <- data.frame()

for (r in rendas) {
  
  umpart <- (sum(((base[[r]])*base$peso_final)[base$renda_irpfepof*12 >= 322295]))/sum(base[[r]]*base$peso_final)
  
  umpc <- rbind(umpc,
                 data.frame("Renda" = r, "Valor" = umpart))
  
}

# Extrair resultados

write.xlsx(dezpc, "dezpc.xlsx", row.names = F)

write.xlsx(umpc, "umpc.xlsx", row.names = F)

write.xlsx(gini_arrecadao, "gini_arrecadacao.xlsx", row.names = F)

