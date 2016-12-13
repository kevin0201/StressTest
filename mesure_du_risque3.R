#Chargement des librairies

library('ggplot2')
library('PerformanceAnalytics')
library('zoo')
library('xts')
library('tseries')
library('corrplot')
library('matrixStats')
library('dygraphs')
library('reshape2')

# Chargement des données

df_airliquide=read.csv('airliquide_jour_2010_2015.csv')

df_axa=read.csv('axa_jour_2010_2015.csv')

df_bnp=read.csv('bnp_jour_2010_2015.csv')

df_compagniesaintgobin=read.csv('compagniesaintgobin_jour_2010_2015.csv')

df_lmvh=read.csv('lmvh_jour_2010_2015.csv')

df_orange=read.csv('orange_jour_2010_2015.csv')

df_sg=read.csv('societegenerale_jour_2010_2015.csv')

df_total=read.csv('total_jour_2010_2015.csv')

df_vinci=read.csv('vinci_jour_2010_2015.csv')

df_creditagricole=read.csv('creditagricole_jour_2010_2015.csv')

date2016 = read.csv('date2016.csv')

# Nombre total d'actifs
nb_actifs= 100

# le portefeuille des actifs; 10 actifs différents.
# Axa,bnp,sg,credit agricole,orange,total,lmvh,csg,vinci,air liquide
portefeuille_parts = c(0.20,0.18,0.18,0.11,0.05,0.099,0.01,0.02,0.12,0.04)

# Portefeuille en nombre d'actifs

portefeuille_nb_actifs = portefeuille_parts*nb_actifs

# Valeurs historiques des actions dans le pportefeuille
#Après avoir chargé les données on ne retient que deux colonnes, la date et la colonne valeur qui est la moyenne des High et Low

df_vairliquide=data.frame(row.names =as.Date(df_airliquide$Date),Valeur=(df_airliquide$High+df_airliquide$Low)/2)

df_vaxa=data.frame(row.names=as.Date(df_axa$Date),Valeur=(df_axa$High+df_axa$Low)/2)

df_vbnp=data.frame(row.names=as.Date(df_bnp$Date),Valeur=(df_bnp$High+df_bnp$Low)/2)

df_vcompagniesaintgobin=data.frame(row.names=as.Date(df_compagniesaintgobin$Date),Valeur=(df_compagniesaintgobin$High+df_compagniesaintgobin$Low)/2)

df_vlmvh=data.frame(row.names=as.Date(df_lmvh$Date),Valeur=(df_lmvh$High+df_lmvh$Low)/2)

df_vorange=data.frame(row.names=as.Date(df_orange$Date),Valeur=(df_orange$High+df_orange$Low)/2)

df_vsg=data.frame(row.names=as.Date(df_sg$Date),Valeur=(df_sg$High+df_sg$Low)/2)

df_vtotal=data.frame(row.names=as.Date(df_total$Date),Valeur=(df_total$High+df_total$Low)/2)

df_vvinci=data.frame(row.names=as.Date(df_vinci$Date),Valeur=(df_vinci$High+df_vinci$Low)/2)

df_vcreditagricole=data.frame(row.names=as.Date(df_creditagricole$Date),Valeur=(df_creditagricole$High+df_creditagricole$Low)/2)

# historique du Portefeuille en terme de valeur total des actifs...
portefeuille_historique=data.frame(row.names =as.Date(df_airliquide$Date),
                                   Valeur=df_vairliquide$Valeur*portefeuille_nb_actifs[10]+
                                     df_vaxa$Valeur*portefeuille_nb_actifs[1]+
                                     df_vbnp$Valeur*portefeuille_nb_actifs[2]+
                                     df_vcompagniesaintgobin$Valeur*portefeuille_nb_actifs[8]+
                                     df_vlmvh$Valeur*portefeuille_nb_actifs[7]+
                                     df_vorange$Valeur*portefeuille_nb_actifs[5]+
                                     df_vsg$Valeur*portefeuille_nb_actifs[3]+
                                     df_vtotal$Valeur*portefeuille_nb_actifs[6]+
                                     df_vvinci$Valeur*portefeuille_nb_actifs[9]+
                                     df_vcreditagricole$Valeur*portefeuille_nb_actifs[4])

# Graphique historique du portefeuille ....

dygraph(as.xts(portefeuille_historique), main = "Evolution historique du cours du PF", 
        ylab = "Cours du PF")


#Poids 
vecteur_poids=data.frame(airliquide=df_vairliquide$Valeur[1]*portefeuille_nb_actifs[10]/portefeuille_historique$Valeur[1],
                         axa=df_vaxa$Valeur[1]*portefeuille_nb_actifs[1]/portefeuille_historique$Valeur[1],
                         bnp=df_vbnp$Valeur[1]*portefeuille_nb_actifs[2]/portefeuille_historique$Valeur[1],
                         csg=df_vcompagniesaintgobin$Valeur[1]*portefeuille_nb_actifs[8]/portefeuille_historique$Valeur[1],
                         crediagricole=df_vcreditagricole$Valeur[1]*portefeuille_nb_actifs[4]/portefeuille_historique$Valeur[1],
                         lmvh=df_vlmvh$Valeur[1]*portefeuille_nb_actifs[7]/portefeuille_historique$Valeur[1],
                         orange=df_vorange$Valeur[1]*portefeuille_nb_actifs[5]/portefeuille_historique$Valeur[1],
                         sg=df_vsg$Valeur[1]*portefeuille_nb_actifs[3]/portefeuille_historique$Valeur[1],
                         total=df_vtotal$Valeur[1]*portefeuille_nb_actifs[6]/portefeuille_historique$Valeur[1],
                         vinci= df_vvinci$Valeur[1]*portefeuille_nb_actifs[9]/portefeuille_historique$Valeur[1])

vecteur_poids = c(vecteur_poids$airliquide[1],
                  vecteur_poids$axa[1],
                  vecteur_poids$bnp[1],
                  vecteur_poids$csg[1],
                  vecteur_poids$crediagricole[1],
                  vecteur_poids$lmvh[1],
                  vecteur_poids$orange[1],
                  vecteur_poids$sg[1],
                  vecteur_poids$total[1],
                  vecteur_poids$vinci[1])


# Rendement des actifs
r_airliquide = na.omit(Return.calculate(df_vairliquide))

r_axa = na.omit(Return.calculate(df_vaxa))

r_bnp = na.omit(Return.calculate(df_vbnp))

r_csg = na.omit(Return.calculate(df_vcompagniesaintgobin))

r_crediagricole = na.omit(Return.calculate(df_vcreditagricole))

r_lmvh = na.omit(Return.calculate(df_vlmvh))

r_orange = na.omit(Return.calculate(df_vorange))

r_sg = na.omit(Return.calculate(df_vsg))

r_total = na.omit(Return.calculate(df_vsg))

r_vinci =na.omit(Return.calculate(df_vvinci))

# Matrice des rendements

matrice_rendement = data.frame(row.names = as.Date(row.names(r_airliquide)),airliquide=r_airliquide$Valeur,
                               axa=r_axa$Valeur, bnp=r_bnp$Valeur, csg=r_csg$Valeur, crediagricole = r_crediagricole$Valeur
                               ,lmvh=r_lmvh$Valeur,orange=r_orange$Valeur,sg=r_sg$Valeur,total=r_total,
                               vinci=r_vinci$Valeur)

# Rendement historique du portefeuille

rendement_histo_port = as.data.frame(Return.portfolio(matrice_rendement,vecteur_poids))

dygraph(as.xts(rendement_histo_port), main = "Evolution historique du rendement du PF", 
        ylab = "Rendement du PF")

# Analyse du portefeuille / rentabilité du portefeuille.
# Rendement moyen du portefeuille

rendement_moy_port = (mean(r_airliquide$Valeur)*vecteur_poids[1]+mean(x = r_axa$Valeur)*vecteur_poids[2]+
  mean(r_bnp$Valeur)*vecteur_poids[3]+
  mean(r_csg$Valeur)*vecteur_poids[4]+
  mean(r_crediagricole$Valeur)*vecteur_poids[5]+
  mean(r_lmvh$Valeur)*vecteur_poids[6]+
  mean(r_orange$Valeur)*vecteur_poids[7]+
  mean(r_sg$Valeur)*vecteur_poids[8]+
  mean(r_total$Valeur)*vecteur_poids[9]+
  mean(r_vinci$Valeur)*vecteur_poids[10])

maxdd=maxDrawdown(rendement_histo_port)

SharpeRatio.annualized(as.xts(rendement_histo_port),Rf=0.007, scale = 255)

mat_correl = cor(matrice_rendement)

mat_covar = cov(matrice_rendement)

# Plot matrice des correlation et des rendements avec ggplot et melt de reshape2

melted_cor = melt(mat_correl)
melted_cov = melt(mat_covar)

ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + labs(title="Matrice des correlations", x="variable 1", y="variable 2")

ggplot(data = melted_cov, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + labs(title="Matrice des covariances", x="variable 1", y="variable 2")

#corrplot(cor(matrice_rendement), type="upper", order="hclust", tl.col="black", tl.srt=45, title = "Matrice des correlations")

### ECARTYPE du portefeuille, 

sigma_portefeuille = vecteur_poids%*%cov(matrice_rendement)%*%vecteur_poids

ratio_sharpe = mean(rendement_moy_port*255-0.007)/(sigma_portefeuille*sqrt(255))


######## Test de normalité ################"

qqplot(quantile(rnorm(1561,mean((as.data.frame(rendement_histo_port))$portfolio.returns),sd=sd((as.data.frame(rendement_histo_port))$portfolio.returns)), probs = seq(0, 0.99975, 1/2000)),
       quantile((as.data.frame(rendement_histo_port))$portfolio.returns, probs = seq(0, 0.99975, 1/2000)),xlab = "quantile théorique", ylab = "quantile empirique", main="Q-rendements et Q-loi normale")

abline(a=0,b=1,col='red')

shapiro.test((as.data.frame(rendement_histo_port))$portfolio.returns)

jarque.bera.test((as.data.frame(rendement_histo_port))$portfolio.returns)


################################## VAR ##########################
# Var historique du porteffeuille 

varhistoriq = VaR(rendement_histo_port,p=0.99,method = "historical")

vargauss = VaR(rendement_histo_port,p=0.99,method = "gaussian")

### Calcul de la CTE ####################""""

t_varhistoriq=mean(rendement_histo_port$portfolio.returns[rendement_histo_port$portfolio.returns<rep(varhistoriq,1561)])



# La fonction black and scholes
# Paramètres
# le taux sans risque r=0.7%

##############################################
# MOUVEMENT BROWNIEN 
#############################################

nsimu=100
ar=0
mat_simu_brownien=matrix(0,261,nsimu)

for(ar in seq(1,nsimu,1)){
  
temps=seq(0,1,length=261)

pas=1/260

Bacc = rnorm(260,sd=sqrt(pas))

# Trajectoire
Bsim = c(0,cumsum(Bacc))

mat_simu_brownien[,ar]=Bsim
  
# plot(temps,Bsim, type="l")

}

test_data_long = mat_simu_brownien[1:115,1:3]
test_data_long <- melt(test_data_long)  # convert to long format

ggplot(data=test_data_long,
       aes(x=Var1, y=value, color=Var2)) + 
  geom_line()+labs(title="Mouvements browniens standard", x="Simulation au temp t", y="Valeur du MB")

# Transformation de la matrice de mouvement brownien en data frame pour ggplot

mat_simu_brownien_df = data.frame(mat_simu_brownien)

########### Projection du prix du portefeuille par ,St de black and scholes #######################"
# S0 = 10; 

i = 0
mat_predict=matrix(0,261,nsimu)

for (i in seq(1,nsimu,1)){
  
  mat_predict[,i]=portefeuille_historique$Valeur[1562]*exp(sd((as.data.frame(rendement_histo_port))$portfolio.returns)*mat_simu_brownien[,i]+mean((as.data.frame(rendement_histo_port))$portfolio.returns)*(1/255))

  }  

# Transormation de la matrice de prediction qui contien les prix projet pour chaque mouvement brownien simulé 

matrice_prix_predict_df = data.frame(row.names = as.Date(date2016$Date),mat_predict)


############### calcul des rendements projetés à partir de la matrice des prix  

Rendement_simu = na.omit(Return.calculate(matrice_prix_predict_df))

var99_predict = data.frame(v=colQuantiles(Rendement_simu, probs=(1-0.99)))

ggplot(var99_predict) +
  geom_line(aes(seq(1,nsimu,1), v)) +
  labs(title="Evolution des VaR à 99% simulées", x="simulation n", y="VaR à 99% par simulation")


varminim = min(var99_predict)

############ Grphique des rendement projetés

ggplot(Rendement_simu) +
  geom_line(aes(seq(1,260,1), X100))+
  labs(title="Evolution des rendements simulées", x="temp t", y="Rendement par simulation")

dygraph(as.xts(Rendement_simu), main = "Evolution du rendement du PF avant Stress", ylab = "Rendement du PF")%>%
  dyLegend(show = "follow")
############ Graphique des volatilites projetés au lieu de supersossé les 100 colonnes de rendement simuléées ####

ev_volatilite_proj=data.frame(v=(apply(as.matrix(Rendement_simu),2,sd)))

ggplot(ev_volatilite_proj) +
  geom_line(aes(seq(1,nsimu,1), v)) +
  labs(title="Evolution des volatilités simulées", x="simulation n", y="Volatilité par simulation")

# Volatilité projetée maximale

vola_proj_max = max(ev_volatilite_proj$v)


##############################################################################

###################################### stressed ##############################

##############################################################################

########### St de black and scholes #######################"
# S0 = 10; 
i = 0
mat_predict2=matrix(0,261,nsimu)

for (i in seq(1,nsimu,1)){
  
  mat_predict2[,i]=portefeuille_historique$Valeur[1562]*exp(sd((as.data.frame(rendement_histo_port)*2)$portfolio.returns)*mat_simu_brownien[,i]+mean((as.data.frame(rendement_histo_port))$portfolio.returns)*0.2*(1/255))

  }  

# Transormation de la matrice de prediction qui contien les prix projet pour chaque mouvement brownien simulé 

matrice_prix_predict_df2 = data.frame(row.names = as.Date(date2016$Date),mat_predict2)

############### calcul des rendements projetés à partir de la matrice des prix  

Rendement_simu2 = na.omit(Return.calculate(matrice_prix_predict_df2))

var99_predict2 = data.frame(v=colQuantiles(Rendement_simu2, probs=(1-0.99)))

ggplot(var99_predict2) +
  geom_line(aes(seq(1,nsimu,1), v)) +
  labs(title="Evolution des VaR à 99% simulées et stressées", x="simulation n", y="VaR à 99% par simulation puis stress")


###########" Calcul des cte par colonnes de rendement simulées ##########################
j=0
mat_var=matrix(0,260,nsimu)
for(j in seq(1,nsimu,1)){
  
  mat_var[,j] = Rendement_simu2[,j]<var99_predict2$v[j]
  
}
mat_var=mat_var*Rendement_simu2

t_var_mat_var = data.frame(v=apply(mat_var,2,mean))

min_tvar_stressed = min(t_var_mat_var)

# Plot de l'évolution des cte

ggplot(t_var_mat_var) +
  geom_line(aes(seq(1,100,1), v))+
  labs(title="Evolution des CTE simulées", x="simulation n", y="TVaR simulation")

#Moyenne des Tvar simulées

moy_tvar_simu = mean(t_var_mat_var$v)

# Var minimal de la simulation de VaR

varminim2 = min(var99_predict2)

############ Grphique des rendement projetés

ggplot(Rendement_simu2) +
  geom_line(aes(seq(1,260,1), X100))+
  labs(title="Evolution des rendements simulées", x="rang", y="Rendement par simulation")

dygraph(as.xts(Rendement_simu2), main = "Evolution du rendement du PF après Stress", ylab = "Rendement du PF")%>%
  dyLegend(show = "follow")

############ Graphique des volatilites projetés au lieu de supersossé les 100 colonnes de rendement simuléées ####

ev_volatilite_proj2=data.frame(v=(apply(as.matrix(Rendement_simu2),2,sd)))

ggplot(ev_volatilite_proj2) +
  geom_line(aes(seq(1,nsimu,1), v)) +
  labs(title="Evolution des volatilités simulées après stress", x="simulation n", y="Volatilité par simulation")

# Volatilité projetée maximale

vola_proj_max2 = max(ev_volatilite_proj2$v)

#########################################################################################################

############################################## DONNEES A PRESENTER ######################################

les_var_presenter = data.frame(t_var_historique = t_varhistoriq,var_historique = varhistoriq, var_min=varminim,var_min_stressed=varminim2)
#les_var_presenter
#t_var_mat_var
#min_tvar_stressed
#