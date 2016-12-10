#Chargement des librairies
library('ggplot2')
library('PerformanceAnalytics')
library('zoo')
library('xts')
library('tseries')
library('corrplot')

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
rendement_histo_port = Return.portfolio(matrice_rendement,vecteur_poids)



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

SharpeRatio.annualized(rendement_histo_port,Rf=0.007, scale = 255)

cor(matrice_rendement)

cov(matrice_rendement)

corrplot(cor(matrice_rendement), type="upper", order="hclust", tl.col="black", tl.srt=45, title = "Matrice des correlations")

### ECARTYPE du portefeuille, 

sigma_portefeuille = vecteur_poids%*%cov(matrice_rendement)%*%vecteur_poids

ratio_sharpe = mean(rendement_moy_port*255-0.007)/(sigma_portefeuille)


######## Test de normalité ################"

qqplot(quantile(rnorm(1561,mean((as.data.frame(rendement_histo_port))$portfolio.returns),sd=sd((as.data.frame(rendement_histo_port))$portfolio.returns)), probs = seq(0, 0.99975, 1/2000)),
       quantile((as.data.frame(rendement_histo_port))$portfolio.returns, probs = seq(0, 0.99975, 1/2000)),xlab = "quantile théorique", ylab = "quantile empirique")

abline(a=0,b=1,col='red')

shapiro.test((as.data.frame(rendement_histo_port))$portfolio.returns)

jarque.bera.test((as.data.frame(rendement_histo_port))$portfolio.returns)


# La fonction black and scholes
# Paramètres
# le taux sans risque r=0.7%

# Moyennes de de l'évolution des différents actifs

moy_air_liquide=mean(df_vairliquide$Valeur/portefeuille_nb_actifs[1])
moy_axa=mean(df_vaxa$Valeur/portefeuille_nb_actifs[2])
moy_bnp=mean(df_vbnp$Valeur/portefeuille_nb_actifs[3])
moy_compagniesaintgobin=mean(df_vcompagniesaintgobin$Valeur/portefeuille_nb_actifs[4])
moy_lmvh=mean(df_vlmvh$Valeur/portefeuille_nb_actifs[5])
moy_orange=mean(df_vorange$Valeur/portefeuille_nb_actifs[6])
moy_sg=mean(df_vsg$Valeur/portefeuille_nb_actifs[7])
moy_total=mean(df_vtotal$Valeur/portefeuille_nb_actifs[8])
moy_vinci=mean(df_vvinci$Valeur/portefeuille_nb_actifs[9])
moy_creditagricole=mean(df_vcreditagricole$Valeur/portefeuille_nb_actifs[10])

# Variances quadratiques
var_air_liquide=var(df_vairliquide$Valeur/portefeuille_nb_actifs[1])
var_axa=var(df_vaxa$Valeur/portefeuille_nb_actifs[2])
var_bnp=var(df_vbnp$Valeur/portefeuille_nb_actifs[3])
var_compagniesaintgobin=var(df_vcompagniesaintgobin$Valeur/portefeuille_nb_actifs[4])
var_lmvh=var(df_vlmvh$Valeur/portefeuille_nb_actifs[5])
var_orange=var(df_vorange$Valeur/portefeuille_nb_actifs[6])
var_sg=var(df_vsg$Valeur/portefeuille_nb_actifs[7])
var_total=var(df_vtotal$Valeur/portefeuille_nb_actifs[8])
var_vinci=var(df_vvinci$Valeur/portefeuille_nb_actifs[9])
var_creditagricole=var(df_vcreditagricole$Valeur/portefeuille_nb_actifs[10])

##############################################
# MOUVEMENT BROWNIEN 
#############################################


##############################################
# Plot, les différentes figures avec ggplot
##############################################

#df=data.frame(x=seq(1,1562,1),y=df_airliquide$Valeur)
#ggplot(df, aes(x, y))+ 
#   geom_line()

