# Pacotes -----------------------------------------------------------------

# Abrir base .dta
library(haven)

# Estatistica ponderada
library(modi)

# Gini ponderado 
library(acid)

# Imposto -----------------------------------------------------------------

base <- read_dta("...")

### Calcular base de calculo a partir do IR atual ###

faixa1=22847.76
faixa2=33919.80
faixa3=45012.60
faixa4=55976.16 

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
  ((faixa4/12)-(faixa3/12))*aliquota4 +
  (base$base_calculo_est[base$base_calculo_est*12 > faixa4]-(faixa4/12))*aliquota4

base$IRPF[is.na(base$IRPF)] <- 0

# Gerar renda disponivel

base$IRPF_pago <- base$IRPF + base$tribut_RTE_trab + base$tribut_RTE_cap

base$renda_disponivel = base$renda_irpfepof - base$IRPF_pago

### Calcular Gini ###

weighted.gini(x = base$renda_disponivel,
              w = base$peso_final)