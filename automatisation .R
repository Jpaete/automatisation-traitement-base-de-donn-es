#packages ---- 
library(readxl)

#imports ---- 
base_dep = 
base_a_dep = attach(base_dep)
base_conditions_com_dep =  
base_conditions_proj_dep = )


#selection etablissements publics ----
#condition 
coeff_etablissements_publics_dep = ifelse(base_conditions_com_dep=="public"|
                                            base_conditions_com_dep=="departement"|
                                            base_conditions_com_dep=="département"|
                                            base_conditions_com_dep=="centrale"|
                                            base_conditions_com_dep=="mines"|
                                            base_conditions_com_dep=="cfa"|
                                            base_conditions_com_dep=="agglo"|
                                            base_conditions_com_dep=="agglomération"|
                                            base_conditions_com_dep=="agglomeration"|
                                            base_conditions_com_dep=="chu"|
                                            base_conditions_com_dep=="lycée"|
                                            base_conditions_com_dep=="lycee"|
                                            base_conditions_com_dep=="collège"|
                                            base_conditions_com_dep=="college"|
                                            base_conditions_com_dep=="commune"|
                                            base_conditions_com_dep=="département"|
                                            base_conditions_com_dep=="departement"|
                                            base_conditions_com_dep=="ecole"|
                                            base_conditions_com_dep=="école"|
                                            base_conditions_com_dep=="cnrs"|
                                            base_conditions_com_dep=="recherche"|
                                            base_conditions_com_dep=="ifremer"|
                                            base_conditions_com_dep=="INRA"|
                                            base_conditions_com_dep=="intra"|
                                            base_conditions_com_dep=="gip"|
                                            base_conditions_com_dep=="GIP"|
                                            base_conditions_com_dep=="inserm"|
                                            base_conditions_com_dep=="irt"|
                                            base_conditions_com_dep=="metropole"|
                                            base_conditions_com_dep=="oniris"|
                                            base_conditions_com_dep=="région"|
                                            base_conditions_com_dep=="region"|
                                            base_conditions_com_dep=="SCNF"|
                                            base_conditions_com_dep=="scnf"|
                                            base_conditions_com_dep=="gare"|
                                            base_conditions_com_dep=="gares"|
                                            base_conditions_com_dep=="mixte"|
                                            base_conditions_com_dep=="université"|
                                            base_conditions_com_dep=="universite"|
                                            base_conditions_com_dep=="villes"|
                                            base_conditions_com_dep=="chambre"|
                                            base_conditions_com_dep=="villes"|
                                            base_conditions_com_dep=="ville"|
                                            base_conditions_com_dep=="publique"|
                                            base_conditions_com_dep=="publiques"|
                                            base_conditions_com_dep=="publics"|
                                            base_conditions_com_dep=="communes"|
                                            base_conditions_com_dep=="inrae"|
                                            base_conditions_com_dep=="d'agglomération"|
                                            base_conditions_com_dep=="d'agglomeration"|
                                            base_conditions_com_dep=="greta"|
                                            base_conditions_com_dep=="office"|
                                            base_conditions_com_dep=="emploi"|
                                            base_conditions_com_dep=="edf"|
                                            base_conditions_com_dep=="gdf"|
                                            base_conditions_com_dep=="communal"|
                                            base_conditions_com_dep=="communales"|
                                            base_conditions_com_dep=="communaux"|
                                            base_conditions_com_dep=="communale"|
                                            base_conditions_com_dep=="cci"|
                                            base_conditions_com_dep=="régional"|
                                            base_conditions_com_dep=="régionale"|
                                            base_conditions_com_dep=="régionales"|
                                            base_conditions_com_dep=="régionaux"|
                                            base_conditions_com_dep=="regional"|
                                            base_conditions_com_dep=="regionale"|
                                            base_conditions_com_dep=="regionales"|
                                            base_conditions_com_dep=="regionaux", 1,0)



##enleve les na 2
coeff_etablissements_publics_dep[is.na(coeff_etablissements_publics_dep)] = 0
##fait la somme des lignes
coeff_etablissements_publics_2_dep=  rowSums(coeff_etablissements_publics_dep)
##selectionne dans le tableau les beneficiaires
beneficiaires_dep = base_a_dep$`Nom du bénéficiaire`


##tableau de verification
etablissements_public_select_dep = data.frame(beneficiaires_dep,coeff_etablissements_publics_2_dep)
View(etablissements_public_select_dep)
##export tableau
write.csv2(etablissements_public_select_dep,"/etablissements_public_select_dep.csv")




#selection projets verts ----
##condition
coeff_projets_verts_dep= ifelse( base_conditions_proj_dep== "développement"|	base_conditions_proj_dep== "développement,"|	base_conditions_proj_dep== "développement."|	base_conditions_proj_dep== "développement)"|	base_conditions_proj_dep== "(développement"|	base_conditions_proj_dep== "(développement)"|	base_conditions_proj_dep== "'développement"|			
                                   base_conditions_proj_dep== "énergétique"|	base_conditions_proj_dep== "énergétique,"|	base_conditions_proj_dep== "énergétique."|	base_conditions_proj_dep== "énergétique)"|	base_conditions_proj_dep== "(énergétique"|	base_conditions_proj_dep== "(énergétique)"|	base_conditions_proj_dep== "'énergétique"|			
                                   base_conditions_proj_dep== "rénovation"|	base_conditions_proj_dep== "rénovation,"|	base_conditions_proj_dep== "rénovation."|	base_conditions_proj_dep== "rénovation)"|	base_conditions_proj_dep== "(rénovation"|	base_conditions_proj_dep== "(rénovation)"|	base_conditions_proj_dep== "'rénovation"|			
                                   base_conditions_proj_dep== "natura"|	base_conditions_proj_dep== "natura,"|	base_conditions_proj_dep== "natura."|	base_conditions_proj_dep== "natura)"|	base_conditions_proj_dep== "(natura"|	base_conditions_proj_dep== "(natura)"|	base_conditions_proj_dep== "'natura"|			
                                   base_conditions_proj_dep== "amélioration"|	base_conditions_proj_dep== "amélioration,"|	base_conditions_proj_dep== "amélioration."|	base_conditions_proj_dep== "amélioration)"|	base_conditions_proj_dep== "(amélioration"|	base_conditions_proj_dep== "(amélioration)"|	base_conditions_proj_dep== "'amélioration"|			
                                   base_conditions_proj_dep== "restauration"|	base_conditions_proj_dep== "restauration,"|	base_conditions_proj_dep== "restauration."|	base_conditions_proj_dep== "restauration)"|	base_conditions_proj_dep== "(restauration"|	base_conditions_proj_dep== "(restauration)"|	base_conditions_proj_dep== "'restauration"|			
                                   base_conditions_proj_dep== "marais"|	base_conditions_proj_dep== "marais,"|	base_conditions_proj_dep== "marais."|	base_conditions_proj_dep== "marais)"|	base_conditions_proj_dep== "(marais"|	base_conditions_proj_dep== "(marais)"|	base_conditions_proj_dep== "'marais"|			
                                   base_conditions_proj_dep== "conservation"|	base_conditions_proj_dep== "conservation,"|	base_conditions_proj_dep== "conservation."|	base_conditions_proj_dep== "conservation)"|	base_conditions_proj_dep== "(conservation"|	base_conditions_proj_dep== "(conservation)"|	base_conditions_proj_dep== "'conservation"|			
                                   base_conditions_proj_dep== "ressources"|	base_conditions_proj_dep== "ressources,"|	base_conditions_proj_dep== "ressources."|	base_conditions_proj_dep== "ressources)"|	base_conditions_proj_dep== "(ressources"|	base_conditions_proj_dep== "(ressources)"|	base_conditions_proj_dep== "'ressources"|			
                                   base_conditions_proj_dep== "réhabilitation"|	base_conditions_proj_dep== "réhabilitation,"|	base_conditions_proj_dep== "réhabilitation."|	base_conditions_proj_dep== "réhabilitation)"|	base_conditions_proj_dep== "(réhabilitation"|	base_conditions_proj_dep== "(réhabilitation)"|	base_conditions_proj_dep== "'réhabilitation"|			
                                   base_conditions_proj_dep== "remplacement"|	base_conditions_proj_dep== "remplacement,"|	base_conditions_proj_dep== "remplacement."|	base_conditions_proj_dep== "remplacement)"|	base_conditions_proj_dep== "(remplacement"|	base_conditions_proj_dep== "(remplacement)"|	base_conditions_proj_dep== "'remplacement"|			
                                   base_conditions_proj_dep== "préservation"|	base_conditions_proj_dep== "préservation,"|	base_conditions_proj_dep== "préservation."|	base_conditions_proj_dep== "préservation)"|	base_conditions_proj_dep== "(préservation"|	base_conditions_proj_dep== "(préservation)"|	base_conditions_proj_dep== "'préservation"|			
                                   base_conditions_proj_dep== "parcs "|	base_conditions_proj_dep== "parcs ,"|	base_conditions_proj_dep== "parcs ."|	base_conditions_proj_dep== "parcs )"|	base_conditions_proj_dep== "(parcs "|	base_conditions_proj_dep== "(parcs )"|	base_conditions_proj_dep== "'parcs "|			
                                   base_conditions_proj_dep== "énergie"|	base_conditions_proj_dep== "énergie,"|	base_conditions_proj_dep== "énergie."|	base_conditions_proj_dep== "énergie)"|	base_conditions_proj_dep== "(énergie"|	base_conditions_proj_dep== "(énergie)"|	base_conditions_proj_dep== "'énergie"|			
                                   base_conditions_proj_dep== "favoriser"|	base_conditions_proj_dep== "favoriser,"|	base_conditions_proj_dep== "favoriser."|	base_conditions_proj_dep== "favoriser)"|	base_conditions_proj_dep== "(favoriser"|	base_conditions_proj_dep== "(favoriser)"|	base_conditions_proj_dep== "'favoriser"|			
                                   base_conditions_proj_dep== "industrielle"|	base_conditions_proj_dep== "industrielle,"|	base_conditions_proj_dep== "industrielle."|	base_conditions_proj_dep== "industrielle)"|	base_conditions_proj_dep== "(industrielle"|	base_conditions_proj_dep== "(industrielle)"|	base_conditions_proj_dep== "'industrielle"|			
                                   base_conditions_proj_dep== "naturels"|	base_conditions_proj_dep== "naturels,"|	base_conditions_proj_dep== "naturels."|	base_conditions_proj_dep== "naturels)"|	base_conditions_proj_dep== "(naturels"|	base_conditions_proj_dep== "(naturels)"|	base_conditions_proj_dep== "'naturels"|			
                                   base_conditions_proj_dep== "quais"|	base_conditions_proj_dep== "quais,"|	base_conditions_proj_dep== "quais."|	base_conditions_proj_dep== "quais)"|	base_conditions_proj_dep== "(quais"|	base_conditions_proj_dep== "(quais)"|	base_conditions_proj_dep== "'quais"|			
                                   base_conditions_proj_dep== "transfromation"|	base_conditions_proj_dep== "transfromation,"|	base_conditions_proj_dep== "transfromation."|	base_conditions_proj_dep== "transfromation)"|	base_conditions_proj_dep== "(transfromation"|	base_conditions_proj_dep== "(transfromation)"|	base_conditions_proj_dep== "'transfromation"|			
                                   base_conditions_proj_dep== "voie "|	base_conditions_proj_dep== "voie ,"|	base_conditions_proj_dep== "voie ."|	base_conditions_proj_dep== "voie )"|	base_conditions_proj_dep== "(voie "|	base_conditions_proj_dep== "(voie )"|	base_conditions_proj_dep== "'voie "|			
                                   base_conditions_proj_dep== "(ges)"|	base_conditions_proj_dep== "(ges),"|	base_conditions_proj_dep== "(ges)."|	base_conditions_proj_dep== "(ges))"|	base_conditions_proj_dep== "((ges)"|	base_conditions_proj_dep== "((ges))"|	base_conditions_proj_dep== "'(ges)"|			
                                   base_conditions_proj_dep== "améliorer"|	base_conditions_proj_dep== "améliorer,"|	base_conditions_proj_dep== "améliorer."|	base_conditions_proj_dep== "améliorer)"|	base_conditions_proj_dep== "(améliorer"|	base_conditions_proj_dep== "(améliorer)"|	base_conditions_proj_dep== "'améliorer"|			
                                   base_conditions_proj_dep== "cyclable"|	base_conditions_proj_dep== "cyclable,"|	base_conditions_proj_dep== "cyclable."|	base_conditions_proj_dep== "cyclable)"|	base_conditions_proj_dep== "(cyclable"|	base_conditions_proj_dep== "(cyclable)"|	base_conditions_proj_dep== "'cyclable"|			
                                   base_conditions_proj_dep== "durable"|	base_conditions_proj_dep== "durable,"|	base_conditions_proj_dep== "durable."|	base_conditions_proj_dep== "durable)"|	base_conditions_proj_dep== "(durable"|	base_conditions_proj_dep== "(durable)"|	base_conditions_proj_dep== "'durable"|			
                                   base_conditions_proj_dep== "itinéraire"|	base_conditions_proj_dep== "itinéraire,"|	base_conditions_proj_dep== "itinéraire."|	base_conditions_proj_dep== "itinéraire)"|	base_conditions_proj_dep== "(itinéraire"|	base_conditions_proj_dep== "(itinéraire)"|	base_conditions_proj_dep== "'itinéraire"|			
                                   base_conditions_proj_dep== "protection"|	base_conditions_proj_dep== "protection,"|	base_conditions_proj_dep== "protection."|	base_conditions_proj_dep== "protection)"|	base_conditions_proj_dep== "(protection"|	base_conditions_proj_dep== "(protection)"|	base_conditions_proj_dep== "'protection"|			
                                   base_conditions_proj_dep== "valorisation"|	base_conditions_proj_dep== "valorisation,"|	base_conditions_proj_dep== "valorisation."|	base_conditions_proj_dep== "valorisation)"|	base_conditions_proj_dep== "(valorisation"|	base_conditions_proj_dep== "(valorisation)"|	base_conditions_proj_dep== "'valorisation"|			
                                   base_conditions_proj_dep== "vélo"|	base_conditions_proj_dep== "vélo,"|	base_conditions_proj_dep== "vélo."|	base_conditions_proj_dep== "vélo)"|	base_conditions_proj_dep== "(vélo"|	base_conditions_proj_dep== "(vélo)"|	base_conditions_proj_dep== "'vélo"|			
                                   base_conditions_proj_dep== "eau"|	base_conditions_proj_dep== "eau,"|	base_conditions_proj_dep== "eau."|	base_conditions_proj_dep== "eau)"|	base_conditions_proj_dep== "(eau"|	base_conditions_proj_dep== "(eau)"|	base_conditions_proj_dep== "'eau"|			
                                   base_conditions_proj_dep== "estuaire"|	base_conditions_proj_dep== "estuaire,"|	base_conditions_proj_dep== "estuaire."|	base_conditions_proj_dep== "estuaire)"|	base_conditions_proj_dep== "(estuaire"|	base_conditions_proj_dep== "(estuaire)"|	base_conditions_proj_dep== "'estuaire"|			
                                   base_conditions_proj_dep== "performance"|	base_conditions_proj_dep== "performance,"|	base_conditions_proj_dep== "performance."|	base_conditions_proj_dep== "performance)"|	base_conditions_proj_dep== "(performance"|	base_conditions_proj_dep== "(performance)"|	base_conditions_proj_dep== "'performance"|			
                                   base_conditions_proj_dep== "aménagement"|	base_conditions_proj_dep== "aménagement,"|	base_conditions_proj_dep== "aménagement."|	base_conditions_proj_dep== "aménagement)"|	base_conditions_proj_dep== "(aménagement"|	base_conditions_proj_dep== "(aménagement)"|	base_conditions_proj_dep== "'aménagement"|			
                                   base_conditions_proj_dep== "isolation"|	base_conditions_proj_dep== "isolation,"|	base_conditions_proj_dep== "isolation."|	base_conditions_proj_dep== "isolation)"|	base_conditions_proj_dep== "(isolation"|	base_conditions_proj_dep== "(isolation)"|	base_conditions_proj_dep== "'isolation"|			
                                   base_conditions_proj_dep== "amélioration"|	base_conditions_proj_dep== "amélioration,"|	base_conditions_proj_dep== "amélioration."|	base_conditions_proj_dep== "amélioration)"|	base_conditions_proj_dep== "(amélioration"|	base_conditions_proj_dep== "(amélioration)"|	base_conditions_proj_dep== "'amélioration"|			
                                   base_conditions_proj_dep== "responsable"|	base_conditions_proj_dep== "responsable,"|	base_conditions_proj_dep== "responsable."|	base_conditions_proj_dep== "responsable)"|	base_conditions_proj_dep== "(responsable"|	base_conditions_proj_dep== "(responsable)"|	base_conditions_proj_dep== "'responsable"|			
                                   base_conditions_proj_dep== "thermique"|	base_conditions_proj_dep== "thermique,"|	base_conditions_proj_dep== "thermique."|	base_conditions_proj_dep== "thermique)"|	base_conditions_proj_dep== "(thermique"|	base_conditions_proj_dep== "(thermique)"|	base_conditions_proj_dep== "'thermique"|			
                                   base_conditions_proj_dep== "bois"|	base_conditions_proj_dep== "bois,"|	base_conditions_proj_dep== "bois."|	base_conditions_proj_dep== "bois)"|	base_conditions_proj_dep== "(bois"|	base_conditions_proj_dep== "(bois)"|	base_conditions_proj_dep== "'bois"|			
                                   base_conditions_proj_dep== "changement"|	base_conditions_proj_dep== "changement,"|	base_conditions_proj_dep== "changement."|	base_conditions_proj_dep== "changement)"|	base_conditions_proj_dep== "(changement"|	base_conditions_proj_dep== "(changement)"|	base_conditions_proj_dep== "'changement"|			
                                   base_conditions_proj_dep== "chaudières"|	base_conditions_proj_dep== "chaudières,"|	base_conditions_proj_dep== "chaudières."|	base_conditions_proj_dep== "chaudières)"|	base_conditions_proj_dep== "(chaudières"|	base_conditions_proj_dep== "(chaudières)"|	base_conditions_proj_dep== "'chaudières"|			
                                   base_conditions_proj_dep== "développement"|	base_conditions_proj_dep== "développement,"|	base_conditions_proj_dep== "développement."|	base_conditions_proj_dep== "développement)"|	base_conditions_proj_dep== "(développement"|	base_conditions_proj_dep== "(développement)"|	base_conditions_proj_dep== "'développement"|			
                                   base_conditions_proj_dep== "énergies"|	base_conditions_proj_dep== "énergies,"|	base_conditions_proj_dep== "énergies."|	base_conditions_proj_dep== "énergies)"|	base_conditions_proj_dep== "(énergies"|	base_conditions_proj_dep== "(énergies)"|	base_conditions_proj_dep== "'énergies"|			
                                   base_conditions_proj_dep== "modernisation"|	base_conditions_proj_dep== "modernisation,"|	base_conditions_proj_dep== "modernisation."|	base_conditions_proj_dep== "modernisation)"|	base_conditions_proj_dep== "(modernisation"|	base_conditions_proj_dep== "(modernisation)"|	base_conditions_proj_dep== "'modernisation"|			
                                   base_conditions_proj_dep== "revitalisation"|	base_conditions_proj_dep== "revitalisation,"|	base_conditions_proj_dep== "revitalisation."|	base_conditions_proj_dep== "revitalisation)"|	base_conditions_proj_dep== "(revitalisation"|	base_conditions_proj_dep== "(revitalisation)"|	base_conditions_proj_dep== "'revitalisation"|			
                                   base_conditions_proj_dep== "sanitaire"|	base_conditions_proj_dep== "sanitaire,"|	base_conditions_proj_dep== "sanitaire."|	base_conditions_proj_dep== "sanitaire)"|	base_conditions_proj_dep== "(sanitaire"|	base_conditions_proj_dep== "(sanitaire)"|	base_conditions_proj_dep== "'sanitaire"|			
                                   base_conditions_proj_dep== "vélo"|	base_conditions_proj_dep== "vélo,"|	base_conditions_proj_dep== "vélo."|	base_conditions_proj_dep== "vélo)"|	base_conditions_proj_dep== "(vélo"|	base_conditions_proj_dep== "(vélo)"|	base_conditions_proj_dep== "'vélo"|			
                                   base_conditions_proj_dep== "ailmentation"|	base_conditions_proj_dep== "ailmentation,"|	base_conditions_proj_dep== "ailmentation."|	base_conditions_proj_dep== "ailmentation)"|	base_conditions_proj_dep== "(ailmentation"|	base_conditions_proj_dep== "(ailmentation)"|	base_conditions_proj_dep== "'ailmentation"|			
                                   base_conditions_proj_dep== "eau"|	base_conditions_proj_dep== "eau,"|	base_conditions_proj_dep== "eau."|	base_conditions_proj_dep== "eau)"|	base_conditions_proj_dep== "(eau"|	base_conditions_proj_dep== "(eau)"|	base_conditions_proj_dep== "'eau"|			
                                   base_conditions_proj_dep== "biopolymères"|	base_conditions_proj_dep== "biopolymères,"|	base_conditions_proj_dep== "biopolymères."|	base_conditions_proj_dep== "biopolymères)"|	base_conditions_proj_dep== "(biopolymères"|	base_conditions_proj_dep== "(biopolymères)"|	base_conditions_proj_dep== "'biopolymères"|			
                                   base_conditions_proj_dep== "aquatique"|	base_conditions_proj_dep== "aquatique,"|	base_conditions_proj_dep== "aquatique."|	base_conditions_proj_dep== "aquatique)"|	base_conditions_proj_dep== "(aquatique"|	base_conditions_proj_dep== "(aquatique)"|	base_conditions_proj_dep== "'aquatique"|			
                                   base_conditions_proj_dep== "chauffage"|	base_conditions_proj_dep== "chauffage,"|	base_conditions_proj_dep== "chauffage."|	base_conditions_proj_dep== "chauffage)"|	base_conditions_proj_dep== "(chauffage"|	base_conditions_proj_dep== "(chauffage)"|	base_conditions_proj_dep== "'chauffage"|			
                                   base_conditions_proj_dep== "cyclables"|	base_conditions_proj_dep== "cyclables,"|	base_conditions_proj_dep== "cyclables."|	base_conditions_proj_dep== "cyclables)"|	base_conditions_proj_dep== "(cyclables"|	base_conditions_proj_dep== "(cyclables)"|	base_conditions_proj_dep== "'cyclables"|			
                                   base_conditions_proj_dep== "efficiente"|	base_conditions_proj_dep== "efficiente,"|	base_conditions_proj_dep== "efficiente."|	base_conditions_proj_dep== "efficiente)"|	base_conditions_proj_dep== "(efficiente"|	base_conditions_proj_dep== "(efficiente)"|	base_conditions_proj_dep== "'efficiente"|			
                                   base_conditions_proj_dep== "lutte"|	base_conditions_proj_dep== "lutte,"|	base_conditions_proj_dep== "lutte."|	base_conditions_proj_dep== "lutte)"|	base_conditions_proj_dep== "(lutte"|	base_conditions_proj_dep== "(lutte)"|	base_conditions_proj_dep== "'lutte"|			
                                   base_conditions_proj_dep== "prévention"|	base_conditions_proj_dep== "prévention,"|	base_conditions_proj_dep== "prévention."|	base_conditions_proj_dep== "prévention)"|	base_conditions_proj_dep== "(prévention"|	base_conditions_proj_dep== "(prévention)"|	base_conditions_proj_dep== "'prévention"|			
                                   base_conditions_proj_dep== "ventilation"|	base_conditions_proj_dep== "ventilation,"|	base_conditions_proj_dep== "ventilation."|	base_conditions_proj_dep== "ventilation)"|	base_conditions_proj_dep== "(ventilation"|	base_conditions_proj_dep== "(ventilation)"|	base_conditions_proj_dep== "'ventilation"|			
                                   base_conditions_proj_dep== "améliorer"|	base_conditions_proj_dep== "améliorer,"|	base_conditions_proj_dep== "améliorer."|	base_conditions_proj_dep== "améliorer)"|	base_conditions_proj_dep== "(améliorer"|	base_conditions_proj_dep== "(améliorer)"|	base_conditions_proj_dep== "'améliorer"|			
                                   base_conditions_proj_dep== "biodiversité"|	base_conditions_proj_dep== "biodiversité,"|	base_conditions_proj_dep== "biodiversité."|	base_conditions_proj_dep== "biodiversité)"|	base_conditions_proj_dep== "(biodiversité"|	base_conditions_proj_dep== "(biodiversité)"|	base_conditions_proj_dep== "'biodiversité"|			
                                   base_conditions_proj_dep== "baie"|	base_conditions_proj_dep== "baie,"|	base_conditions_proj_dep== "baie."|	base_conditions_proj_dep== "baie)"|	base_conditions_proj_dep== "(baie"|	base_conditions_proj_dep== "(baie)"|	base_conditions_proj_dep== "'baie"|			
                                   base_conditions_proj_dep== "canaux"|	base_conditions_proj_dep== "canaux,"|	base_conditions_proj_dep== "canaux."|	base_conditions_proj_dep== "canaux)"|	base_conditions_proj_dep== "(canaux"|	base_conditions_proj_dep== "(canaux)"|	base_conditions_proj_dep== "'canaux"|			
                                   base_conditions_proj_dep== "conforme"|	base_conditions_proj_dep== "conforme,"|	base_conditions_proj_dep== "conforme."|	base_conditions_proj_dep== "conforme)"|	base_conditions_proj_dep== "(conforme"|	base_conditions_proj_dep== "(conforme)"|	base_conditions_proj_dep== "'conforme"|			
                                   base_conditions_proj_dep== "douce"|	base_conditions_proj_dep== "douce,"|	base_conditions_proj_dep== "douce."|	base_conditions_proj_dep== "douce)"|	base_conditions_proj_dep== "(douce"|	base_conditions_proj_dep== "(douce)"|	base_conditions_proj_dep== "'douce"|			
                                   base_conditions_proj_dep== "écologique"|	base_conditions_proj_dep== "écologique,"|	base_conditions_proj_dep== "écologique."|	base_conditions_proj_dep== "écologique)"|	base_conditions_proj_dep== "(écologique"|	base_conditions_proj_dep== "(écologique)"|	base_conditions_proj_dep== "'écologique"|			
                                   base_conditions_proj_dep== "énergétique "|	base_conditions_proj_dep== "énergétique ,"|	base_conditions_proj_dep== "énergétique ."|	base_conditions_proj_dep== "énergétique )"|	base_conditions_proj_dep== "(énergétique "|	base_conditions_proj_dep== "(énergétique )"|	base_conditions_proj_dep== "'énergétique "|			
                                   base_conditions_proj_dep== "energie"|	base_conditions_proj_dep== "energie,"|	base_conditions_proj_dep== "energie."|	base_conditions_proj_dep== "energie)"|	base_conditions_proj_dep== "(energie"|	base_conditions_proj_dep== "(energie)"|	base_conditions_proj_dep== "'energie"|			
                                   base_conditions_proj_dep== "fleuve"|	base_conditions_proj_dep== "fleuve,"|	base_conditions_proj_dep== "fleuve."|	base_conditions_proj_dep== "fleuve)"|	base_conditions_proj_dep== "(fleuve"|	base_conditions_proj_dep== "(fleuve)"|	base_conditions_proj_dep== "'fleuve"|			
                                   base_conditions_proj_dep== "kwhep/m²/an"|	base_conditions_proj_dep== "kwhep/m²/an,"|	base_conditions_proj_dep== "kwhep/m²/an."|	base_conditions_proj_dep== "kwhep/m²/an)"|	base_conditions_proj_dep== "(kwhep/m²/an"|	base_conditions_proj_dep== "(kwhep/m²/an)"|	base_conditions_proj_dep== "'kwhep/m²/an"|			
                                   base_conditions_proj_dep== "marine"|	base_conditions_proj_dep== "marine,"|	base_conditions_proj_dep== "marine."|	base_conditions_proj_dep== "marine)"|	base_conditions_proj_dep== "(marine"|	base_conditions_proj_dep== "(marine)"|	base_conditions_proj_dep== "'marine"|			
                                   base_conditions_proj_dep== "mer"|	base_conditions_proj_dep== "mer,"|	base_conditions_proj_dep== "mer."|	base_conditions_proj_dep== "mer)"|	base_conditions_proj_dep== "(mer"|	base_conditions_proj_dep== "(mer)"|	base_conditions_proj_dep== "'mer"|			
                                   base_conditions_proj_dep== "multimodal"|	base_conditions_proj_dep== "multimodal,"|	base_conditions_proj_dep== "multimodal."|	base_conditions_proj_dep== "multimodal)"|	base_conditions_proj_dep== "(multimodal"|	base_conditions_proj_dep== "(multimodal)"|	base_conditions_proj_dep== "'multimodal"|			
                                   base_conditions_proj_dep== "normes"|	base_conditions_proj_dep== "normes,"|	base_conditions_proj_dep== "normes."|	base_conditions_proj_dep== "normes)"|	base_conditions_proj_dep== "(normes"|	base_conditions_proj_dep== "(normes)"|	base_conditions_proj_dep== "'normes"|			
                                   base_conditions_proj_dep== "nucléaire"|	base_conditions_proj_dep== "nucléaire,"|	base_conditions_proj_dep== "nucléaire."|	base_conditions_proj_dep== "nucléaire)"|	base_conditions_proj_dep== "(nucléaire"|	base_conditions_proj_dep== "(nucléaire)"|	base_conditions_proj_dep== "'nucléaire"|			
                                   base_conditions_proj_dep== "performances"|	base_conditions_proj_dep== "performances,"|	base_conditions_proj_dep== "performances."|	base_conditions_proj_dep== "performances)"|	base_conditions_proj_dep== "(performances"|	base_conditions_proj_dep== "(performances)"|	base_conditions_proj_dep== "'performances"|			
                                   base_conditions_proj_dep== "protéger"|	base_conditions_proj_dep== "protéger,"|	base_conditions_proj_dep== "protéger."|	base_conditions_proj_dep== "protéger)"|	base_conditions_proj_dep== "(protéger"|	base_conditions_proj_dep== "(protéger)"|	base_conditions_proj_dep== "'protéger"|			
                                   base_conditions_proj_dep== "protégés"|	base_conditions_proj_dep== "protégés,"|	base_conditions_proj_dep== "protégés."|	base_conditions_proj_dep== "protégés)"|	base_conditions_proj_dep== "(protégés"|	base_conditions_proj_dep== "(protégés)"|	base_conditions_proj_dep== "'protégés"|			
                                   base_conditions_proj_dep== "renouvelables"|	base_conditions_proj_dep== "renouvelables,"|	base_conditions_proj_dep== "renouvelables."|	base_conditions_proj_dep== "renouvelables)"|	base_conditions_proj_dep== "(renouvelables"|	base_conditions_proj_dep== "(renouvelables)"|	base_conditions_proj_dep== "'renouvelables"|			
                                   base_conditions_proj_dep== "sensibilation"|	base_conditions_proj_dep== "sensibilation,"|	base_conditions_proj_dep== "sensibilation."|	base_conditions_proj_dep== "sensibilation)"|	base_conditions_proj_dep== "(sensibilation"|	base_conditions_proj_dep== "(sensibilation)"|	base_conditions_proj_dep== "'sensibilation"|			
                                   base_conditions_proj_dep== "sensibiliser"|	base_conditions_proj_dep== "sensibiliser,"|	base_conditions_proj_dep== "sensibiliser."|	base_conditions_proj_dep== "sensibiliser)"|	base_conditions_proj_dep== "(sensibiliser"|	base_conditions_proj_dep== "(sensibiliser)"|	base_conditions_proj_dep== "'sensibiliser"|			
                                   base_conditions_proj_dep== "thermostatiques"|	base_conditions_proj_dep== "thermostatiques,"|	base_conditions_proj_dep== "thermostatiques."|	base_conditions_proj_dep== "thermostatiques)"|	base_conditions_proj_dep== "(thermostatiques"|	base_conditions_proj_dep== "(thermostatiques)"|	base_conditions_proj_dep== "'thermostatiques"|			
                                   base_conditions_proj_dep== "ventilation"|	base_conditions_proj_dep== "ventilation,"|	base_conditions_proj_dep== "ventilation."|	base_conditions_proj_dep== "ventilation)"|	base_conditions_proj_dep== "(ventilation"|	base_conditions_proj_dep== "(ventilation)"|	base_conditions_proj_dep== "'ventilation"|			
                                   base_conditions_proj_dep== "verte"|	base_conditions_proj_dep== "verte,"|	base_conditions_proj_dep== "verte."|	base_conditions_proj_dep== "verte)"|	base_conditions_proj_dep== "(verte"|	base_conditions_proj_dep== "(verte)"|	base_conditions_proj_dep== "'verte"|			
                                   base_conditions_proj_dep== "vertes"|	base_conditions_proj_dep== "vertes,"|	base_conditions_proj_dep== "vertes."|	base_conditions_proj_dep== "vertes)"|	base_conditions_proj_dep== "(vertes"|	base_conditions_proj_dep== "(vertes)"|	base_conditions_proj_dep== "'vertes"|			
                                   base_conditions_proj_dep== "électricité"|	base_conditions_proj_dep== "électricité,"|	base_conditions_proj_dep== "électricité."|	base_conditions_proj_dep== "électricité)"|	base_conditions_proj_dep== "(électricité"|	base_conditions_proj_dep== "(électricité)"|	base_conditions_proj_dep== "'électricité"|			
                                   base_conditions_proj_dep== "énergie"|	base_conditions_proj_dep== "énergie,"|	base_conditions_proj_dep== "énergie."|	base_conditions_proj_dep== "énergie)"|	base_conditions_proj_dep== "(énergie"|	base_conditions_proj_dep== "(énergie)"|	base_conditions_proj_dep== "'énergie"|			
                                   base_conditions_proj_dep== "environnement"|	base_conditions_proj_dep== "environnement,"|	base_conditions_proj_dep== "environnement."|	base_conditions_proj_dep== "environnement)"|	base_conditions_proj_dep== "(environnement"|	base_conditions_proj_dep== "(environnement)"|	base_conditions_proj_dep== "'environnement"|			
                                   base_conditions_proj_dep== "insectes"|	base_conditions_proj_dep== "insectes,"|	base_conditions_proj_dep== "insectes."|	base_conditions_proj_dep== "insectes)"|	base_conditions_proj_dep== "(insectes"|	base_conditions_proj_dep== "(insectes)"|	base_conditions_proj_dep== "'insectes"|			
                                   base_conditions_proj_dep== "(ges)"|	base_conditions_proj_dep== "(ges),"|	base_conditions_proj_dep== "(ges)."|	base_conditions_proj_dep== "(ges))"|	base_conditions_proj_dep== "((ges)"|	base_conditions_proj_dep== "((ges))"|	base_conditions_proj_dep== "'(ges)"|			
                                   base_conditions_proj_dep== "transition"|	base_conditions_proj_dep== "transition,"|	base_conditions_proj_dep== "transition."|	base_conditions_proj_dep== "transition)"|	base_conditions_proj_dep== "(transition"|	base_conditions_proj_dep== "(transition)"|	base_conditions_proj_dep== "'transition"|			
                                   base_conditions_proj_dep== "biomarqueurs"|	base_conditions_proj_dep== "biomarqueurs,"|	base_conditions_proj_dep== "biomarqueurs."|	base_conditions_proj_dep== "biomarqueurs)"|	base_conditions_proj_dep== "(biomarqueurs"|	base_conditions_proj_dep== "(biomarqueurs)"|	base_conditions_proj_dep== "'biomarqueurs"|			
                                   base_conditions_proj_dep== "bioregate"|	base_conditions_proj_dep== "bioregate,"|	base_conditions_proj_dep== "bioregate."|	base_conditions_proj_dep== "bioregate)"|	base_conditions_proj_dep== "(bioregate"|	base_conditions_proj_dep== "(bioregate)"|	base_conditions_proj_dep== "'bioregate"|			
                                   base_conditions_proj_dep== "biothérapies"|	base_conditions_proj_dep== "biothérapies,"|	base_conditions_proj_dep== "biothérapies."|	base_conditions_proj_dep== "biothérapies)"|	base_conditions_proj_dep== "(biothérapies"|	base_conditions_proj_dep== "(biothérapies)"|	base_conditions_proj_dep== "'biothérapies"|			
                                   base_conditions_proj_dep== "chaleur"|	base_conditions_proj_dep== "chaleur,"|	base_conditions_proj_dep== "chaleur."|	base_conditions_proj_dep== "chaleur)"|	base_conditions_proj_dep== "(chaleur"|	base_conditions_proj_dep== "(chaleur)"|	base_conditions_proj_dep== "'chaleur"|			
                                   base_conditions_proj_dep== "co2"|	base_conditions_proj_dep== "co2,"|	base_conditions_proj_dep== "co2."|	base_conditions_proj_dep== "co2)"|	base_conditions_proj_dep== "(co2"|	base_conditions_proj_dep== "(co2)"|	base_conditions_proj_dep== "'co2"|			
                                   base_conditions_proj_dep== "CO2"|	base_conditions_proj_dep== "CO2,"|	base_conditions_proj_dep== "CO2."|	base_conditions_proj_dep== "CO2)"|	base_conditions_proj_dep== "(CO2"|	base_conditions_proj_dep== "(CO2)"|	base_conditions_proj_dep== "'CO2"|			
                                   base_conditions_proj_dep== "condensation"|	base_conditions_proj_dep== "condensation,"|	base_conditions_proj_dep== "condensation."|	base_conditions_proj_dep== "condensation)"|	base_conditions_proj_dep== "(condensation"|	base_conditions_proj_dep== "(condensation)"|	base_conditions_proj_dep== "'condensation"|			
                                   base_conditions_proj_dep== "cyclistes"|	base_conditions_proj_dep== "cyclistes,"|	base_conditions_proj_dep== "cyclistes."|	base_conditions_proj_dep== "cyclistes)"|	base_conditions_proj_dep== "(cyclistes"|	base_conditions_proj_dep== "(cyclistes)"|	base_conditions_proj_dep== "'cyclistes"|			
                                   base_conditions_proj_dep== "déplacements"|	base_conditions_proj_dep== "déplacements,"|	base_conditions_proj_dep== "déplacements."|	base_conditions_proj_dep== "déplacements)"|	base_conditions_proj_dep== "(déplacements"|	base_conditions_proj_dep== "(déplacements)"|	base_conditions_proj_dep== "'déplacements"|			
                                   base_conditions_proj_dep== "diversité"|	base_conditions_proj_dep== "diversité,"|	base_conditions_proj_dep== "diversité."|	base_conditions_proj_dep== "diversité)"|	base_conditions_proj_dep== "(diversité"|	base_conditions_proj_dep== "(diversité)"|	base_conditions_proj_dep== "'diversité"|			
                                   base_conditions_proj_dep== "doux "|	base_conditions_proj_dep== "doux ,"|	base_conditions_proj_dep== "doux ."|	base_conditions_proj_dep== "doux )"|	base_conditions_proj_dep== "(doux "|	base_conditions_proj_dep== "(doux )"|	base_conditions_proj_dep== "'doux "|			
                                   base_conditions_proj_dep== "eaux"|	base_conditions_proj_dep== "eaux,"|	base_conditions_proj_dep== "eaux."|	base_conditions_proj_dep== "eaux)"|	base_conditions_proj_dep== "(eaux"|	base_conditions_proj_dep== "(eaux)"|	base_conditions_proj_dep== "'eaux"|			
                                   base_conditions_proj_dep== "électrique"|	base_conditions_proj_dep== "électrique,"|	base_conditions_proj_dep== "électrique."|	base_conditions_proj_dep== "électrique)"|	base_conditions_proj_dep== "(électrique"|	base_conditions_proj_dep== "(électrique)"|	base_conditions_proj_dep== "'électrique"|			
                                   base_conditions_proj_dep== "énergétiques"|	base_conditions_proj_dep== "énergétiques,"|	base_conditions_proj_dep== "énergétiques."|	base_conditions_proj_dep== "énergétiques)"|	base_conditions_proj_dep== "(énergétiques"|	base_conditions_proj_dep== "(énergétiques)"|	base_conditions_proj_dep== "'énergétiques"|			
                                   base_conditions_proj_dep== "énergies"|	base_conditions_proj_dep== "énergies,"|	base_conditions_proj_dep== "énergies."|	base_conditions_proj_dep== "énergies)"|	base_conditions_proj_dep== "(énergies"|	base_conditions_proj_dep== "(énergies)"|	base_conditions_proj_dep== "'énergies"|			
                                   base_conditions_proj_dep== "environnemental"|	base_conditions_proj_dep== "environnemental,"|	base_conditions_proj_dep== "environnemental."|	base_conditions_proj_dep== "environnemental)"|	base_conditions_proj_dep== "(environnemental"|	base_conditions_proj_dep== "(environnemental)"|	base_conditions_proj_dep== "'environnemental"|			
                                   base_conditions_proj_dep== "environnementaux"|	base_conditions_proj_dep== "environnementaux,"|	base_conditions_proj_dep== "environnementaux."|	base_conditions_proj_dep== "environnementaux)"|	base_conditions_proj_dep== "(environnementaux"|	base_conditions_proj_dep== "(environnementaux)"|	base_conditions_proj_dep== "'environnementaux"|			
                                   base_conditions_proj_dep== "gares"|	base_conditions_proj_dep== "gares,"|	base_conditions_proj_dep== "gares."|	base_conditions_proj_dep== "gares)"|	base_conditions_proj_dep== "(gares"|	base_conditions_proj_dep== "(gares)"|	base_conditions_proj_dep== "'gares"|			
                                   base_conditions_proj_dep== "green"|	base_conditions_proj_dep== "green,"|	base_conditions_proj_dep== "green."|	base_conditions_proj_dep== "green)"|	base_conditions_proj_dep== "(green"|	base_conditions_proj_dep== "(green)"|	base_conditions_proj_dep== "'green"|			
                                   base_conditions_proj_dep== "humides"|	base_conditions_proj_dep== "humides,"|	base_conditions_proj_dep== "humides."|	base_conditions_proj_dep== "humides)"|	base_conditions_proj_dep== "(humides"|	base_conditions_proj_dep== "(humides)"|	base_conditions_proj_dep== "'humides"|			
                                   base_conditions_proj_dep== "lutter"|	base_conditions_proj_dep== "lutter,"|	base_conditions_proj_dep== "lutter."|	base_conditions_proj_dep== "lutter)"|	base_conditions_proj_dep== "(lutter"|	base_conditions_proj_dep== "(lutter)"|	base_conditions_proj_dep== "'lutter"|			
                                   base_conditions_proj_dep== "mobilités"|	base_conditions_proj_dep== "mobilités,"|	base_conditions_proj_dep== "mobilités."|	base_conditions_proj_dep== "mobilités)"|	base_conditions_proj_dep== "(mobilités"|	base_conditions_proj_dep== "(mobilités)"|	base_conditions_proj_dep== "'mobilités"|			
                                   base_conditions_proj_dep== "mobilité"|	base_conditions_proj_dep== "mobilité,"|	base_conditions_proj_dep== "mobilité."|	base_conditions_proj_dep== "mobilité)"|	base_conditions_proj_dep== "(mobilité"|	base_conditions_proj_dep== "(mobilité)"|	base_conditions_proj_dep== "'mobilité"|			
                                   base_conditions_proj_dep== "renouvelables"|	base_conditions_proj_dep== "renouvelables,"|	base_conditions_proj_dep== "renouvelables."|	base_conditions_proj_dep== "renouvelables)"|	base_conditions_proj_dep== "(renouvelables"|	base_conditions_proj_dep== "(renouvelables)"|	base_conditions_proj_dep== "'renouvelables"|			
                                   base_conditions_proj_dep== "restaurer"|	base_conditions_proj_dep== "restaurer,"|	base_conditions_proj_dep== "restaurer."|	base_conditions_proj_dep== "restaurer)"|	base_conditions_proj_dep== "(restaurer"|	base_conditions_proj_dep== "(restaurer)"|	base_conditions_proj_dep== "'restaurer"|			
                                   base_conditions_proj_dep== "substances"|	base_conditions_proj_dep== "substances,"|	base_conditions_proj_dep== "substances."|	base_conditions_proj_dep== "substances)"|	base_conditions_proj_dep== "(substances"|	base_conditions_proj_dep== "(substances)"|	base_conditions_proj_dep== "'substances"|			
                                   base_conditions_proj_dep== "alimentation"|	base_conditions_proj_dep== "alimentation,"|	base_conditions_proj_dep== "alimentation."|	base_conditions_proj_dep== "alimentation)"|	base_conditions_proj_dep== "(alimentation"|	base_conditions_proj_dep== "(alimentation)"|	base_conditions_proj_dep== "'alimentation"|			
                                   base_conditions_proj_dep== "norme"|	base_conditions_proj_dep== "norme,"|	base_conditions_proj_dep== "norme."|	base_conditions_proj_dep== "norme)"|	base_conditions_proj_dep== "(norme"|	base_conditions_proj_dep== "(norme)"|	base_conditions_proj_dep== "'norme"|			
                                   base_conditions_proj_dep== "adaptation"|	base_conditions_proj_dep== "adaptation,"|	base_conditions_proj_dep== "adaptation."|	base_conditions_proj_dep== "adaptation)"|	base_conditions_proj_dep== "(adaptation"|	base_conditions_proj_dep== "(adaptation)"|	base_conditions_proj_dep== "'adaptation"|			
                                   base_conditions_proj_dep== "recyclage"|	base_conditions_proj_dep== "recyclage,"|	base_conditions_proj_dep== "recyclage."|	base_conditions_proj_dep== "recyclage)"|	base_conditions_proj_dep== "(recyclage"|	base_conditions_proj_dep== "(recyclage)"|	base_conditions_proj_dep== "'recyclage"|			
                                   base_conditions_proj_dep== "biologique"|	base_conditions_proj_dep== "biologique,"|	base_conditions_proj_dep== "biologique."|	base_conditions_proj_dep== "biologique)"|	base_conditions_proj_dep== "(biologique"|	base_conditions_proj_dep== "(biologique)"|	base_conditions_proj_dep== "'biologique"|			
                                   base_conditions_proj_dep== "biologiques"|	base_conditions_proj_dep== "biologiques,"|	base_conditions_proj_dep== "biologiques."|	base_conditions_proj_dep== "biologiques)"|	base_conditions_proj_dep== "(biologiques"|	base_conditions_proj_dep== "(biologiques)"|	base_conditions_proj_dep== "'biologiques"|			
                                   base_conditions_proj_dep== "biologie"|	base_conditions_proj_dep== "biologie,"|	base_conditions_proj_dep== "biologie."|	base_conditions_proj_dep== "biologie)"|	base_conditions_proj_dep== "(biologie"|	base_conditions_proj_dep== "(biologie)"|	base_conditions_proj_dep== "'biologie"|			
                                   base_conditions_proj_dep== "bioressources"|	base_conditions_proj_dep== "bioressources,"|	base_conditions_proj_dep== "bioressources."|	base_conditions_proj_dep== "bioressources)"|	base_conditions_proj_dep== "(bioressources"|	base_conditions_proj_dep== "(bioressources)"|	base_conditions_proj_dep== "'bioressources"|			
                                   base_conditions_proj_dep== "climatique"|	base_conditions_proj_dep== "climatique,"|	base_conditions_proj_dep== "climatique."|	base_conditions_proj_dep== "climatique)"|	base_conditions_proj_dep== "(climatique"|	base_conditions_proj_dep== "(climatique)"|	base_conditions_proj_dep== "'climatique"|			
                                   base_conditions_proj_dep== "écologiques"|	base_conditions_proj_dep== "écologiques,"|	base_conditions_proj_dep== "écologiques."|	base_conditions_proj_dep== "écologiques)"|	base_conditions_proj_dep== "(écologiques"|	base_conditions_proj_dep== "(écologiques)"|	base_conditions_proj_dep== "'écologiques"|			
                                   base_conditions_proj_dep== "énergie/environnement"|	base_conditions_proj_dep== "énergie/environnement,"|	base_conditions_proj_dep== "énergie/environnement."|	base_conditions_proj_dep== "énergie/environnement)"|	base_conditions_proj_dep== "(énergie/environnement"|	base_conditions_proj_dep== "(énergie/environnement)"|	base_conditions_proj_dep== "'énergie/environnement"|			
                                   base_conditions_proj_dep== "éoliens"|	base_conditions_proj_dep== "éoliens,"|	base_conditions_proj_dep== "éoliens."|	base_conditions_proj_dep== "éoliens)"|	base_conditions_proj_dep== "(éoliens"|	base_conditions_proj_dep== "(éoliens)"|	base_conditions_proj_dep== "'éoliens"|			
                                   base_conditions_proj_dep== "eurovélo"|	base_conditions_proj_dep== "eurovélo,"|	base_conditions_proj_dep== "eurovélo."|	base_conditions_proj_dep== "eurovélo)"|	base_conditions_proj_dep== "(eurovélo"|	base_conditions_proj_dep== "(eurovélo)"|	base_conditions_proj_dep== "'eurovélo"|			
                                   base_conditions_proj_dep== "éolien"|	base_conditions_proj_dep== "éolien,"|	base_conditions_proj_dep== "éolien."|	base_conditions_proj_dep== "éolien)"|	base_conditions_proj_dep== "(éolien"|	base_conditions_proj_dep== "(éolien)"|	base_conditions_proj_dep== "'éolien"|			
                                   base_conditions_proj_dep== "eurovéloroute"|	base_conditions_proj_dep== "eurovéloroute,"|	base_conditions_proj_dep== "eurovéloroute."|	base_conditions_proj_dep== "eurovéloroute)"|	base_conditions_proj_dep== "(eurovéloroute"|	base_conditions_proj_dep== "(eurovéloroute)"|	base_conditions_proj_dep== "'eurovéloroute"|			
                                   base_conditions_proj_dep== "hydraulique"|	base_conditions_proj_dep== "hydraulique,"|	base_conditions_proj_dep== "hydraulique."|	base_conditions_proj_dep== "hydraulique)"|	base_conditions_proj_dep== "(hydraulique"|	base_conditions_proj_dep== "(hydraulique)"|	base_conditions_proj_dep== "'hydraulique"|			
                                   base_conditions_proj_dep== "hydrauliques"|	base_conditions_proj_dep== "hydrauliques,"|	base_conditions_proj_dep== "hydrauliques."|	base_conditions_proj_dep== "hydrauliques)"|	base_conditions_proj_dep== "(hydrauliques"|	base_conditions_proj_dep== "(hydrauliques)"|	base_conditions_proj_dep== "'hydrauliques"|			
                                   base_conditions_proj_dep== "l'écosystème"|	base_conditions_proj_dep== "l'écosystème,"|	base_conditions_proj_dep== "l'écosystème."|	base_conditions_proj_dep== "l'écosystème)"|	base_conditions_proj_dep== "(l'écosystème"|	base_conditions_proj_dep== "(l'écosystème)"|	base_conditions_proj_dep== "'l'écosystème"|			
                                   base_conditions_proj_dep== "l'environnement"|	base_conditions_proj_dep== "l'environnement,"|	base_conditions_proj_dep== "l'environnement."|	base_conditions_proj_dep== "l'environnement)"|	base_conditions_proj_dep== "(l'environnement"|	base_conditions_proj_dep== "(l'environnement)"|	base_conditions_proj_dep== "'l'environnement"|			
                                   base_conditions_proj_dep== "quai"|	base_conditions_proj_dep== "quai,"|	base_conditions_proj_dep== "quai."|	base_conditions_proj_dep== "quai)"|	base_conditions_proj_dep== "(quai"|	base_conditions_proj_dep== "(quai)"|	base_conditions_proj_dep== "'quai"|			
                                   base_conditions_proj_dep== "température"|	base_conditions_proj_dep== "température,"|	base_conditions_proj_dep== "température."|	base_conditions_proj_dep== "température)"|	base_conditions_proj_dep== "(température"|	base_conditions_proj_dep== "(température)"|	base_conditions_proj_dep== "'température"|			
                                   base_conditions_proj_dep== "températures"|	base_conditions_proj_dep== "températures,"|	base_conditions_proj_dep== "températures."|	base_conditions_proj_dep== "températures)"|	base_conditions_proj_dep== "(températures"|	base_conditions_proj_dep== "(températures)"|	base_conditions_proj_dep== "'températures"|			
                                   base_conditions_proj_dep== "végétale"|	base_conditions_proj_dep== "végétale,"|	base_conditions_proj_dep== "végétale."|	base_conditions_proj_dep== "végétale)"|	base_conditions_proj_dep== "(végétale"|	base_conditions_proj_dep== "(végétale)"|	base_conditions_proj_dep== "'végétale"|			
                                   base_conditions_proj_dep== "végétaux"|	base_conditions_proj_dep== "végétaux,"|	base_conditions_proj_dep== "végétaux."|	base_conditions_proj_dep== "végétaux)"|	base_conditions_proj_dep== "(végétaux"|	base_conditions_proj_dep== "(végétaux)"|	base_conditions_proj_dep== "'végétaux"|			
                                   base_conditions_proj_dep== "végétal"|	base_conditions_proj_dep== "végétal,"|	base_conditions_proj_dep== "végétal."|	base_conditions_proj_dep== "végétal)"|	base_conditions_proj_dep== "(végétal"|	base_conditions_proj_dep== "(végétal)"|	base_conditions_proj_dep== "'végétal"|			
                                   base_conditions_proj_dep== "végétales"|	base_conditions_proj_dep== "végétales,"|	base_conditions_proj_dep== "végétales."|	base_conditions_proj_dep== "végétales)"|	base_conditions_proj_dep== "(végétales"|	base_conditions_proj_dep== "(végétales)"|	base_conditions_proj_dep== "'végétales"|			
                                   base_conditions_proj_dep== "végétation"|	base_conditions_proj_dep== "végétation,"|	base_conditions_proj_dep== "végétation."|	base_conditions_proj_dep== "végétation)"|	base_conditions_proj_dep== "(végétation"|	base_conditions_proj_dep== "(végétation)"|	base_conditions_proj_dep== "'végétation"|			
                                   base_conditions_proj_dep== "vélo"|	base_conditions_proj_dep== "vélo,"|	base_conditions_proj_dep== "vélo."|	base_conditions_proj_dep== "vélo)"|	base_conditions_proj_dep== "(vélo"|	base_conditions_proj_dep== "(vélo)"|	base_conditions_proj_dep== "'vélo"|			
                                   base_conditions_proj_dep== "véloroutes"|	base_conditions_proj_dep== "véloroutes,"|	base_conditions_proj_dep== "véloroutes."|	base_conditions_proj_dep== "véloroutes)"|	base_conditions_proj_dep== "(véloroutes"|	base_conditions_proj_dep== "(véloroutes)"|	base_conditions_proj_dep== "'véloroutes"|			
                                   base_conditions_proj_dep== "véloroute"|	base_conditions_proj_dep== "véloroute,"|	base_conditions_proj_dep== "véloroute."|	base_conditions_proj_dep== "véloroute)"|	base_conditions_proj_dep== "(véloroute"|	base_conditions_proj_dep== "(véloroute)"|	base_conditions_proj_dep== "'véloroute"|			
                                   base_conditions_proj_dep== "veloroute"|	base_conditions_proj_dep== "veloroute,"|	base_conditions_proj_dep== "veloroute."|	base_conditions_proj_dep== "veloroute)"|	base_conditions_proj_dep== "(veloroute"|	base_conditions_proj_dep== "(veloroute)"|	base_conditions_proj_dep== "'veloroute"|			
                                   base_conditions_proj_dep== "éolien"|	base_conditions_proj_dep== "éolien,"|	base_conditions_proj_dep== "éolien."|	base_conditions_proj_dep== "éolien)"|	base_conditions_proj_dep== "(éolien"|	base_conditions_proj_dep== "(éolien)"|	base_conditions_proj_dep== "'éolien" ,1,0)

##enleve les na 2 ----
coeff_projets_verts_dep[is.na(coeff_projets_verts_dep)] = 0

##fait la somme des lignes
coeff_projets_verts_2_dep =  rowSums(coeff_projets_verts_dep )

##selectionne dans le tableau les projets
projets_dep = base_a_dep$`Intitulé du projet`
descr_dep = base_a_dep$`Résumé de l'opération`

##tableau de verification
projets_select_dep = data.frame(projets_dep,descr_dep,coeff_projets_verts_2_dep)
##export tableau
write.csv2(projets_select_dep,"/projet_select_dep.csv" )
#merge des différents tableaux ----
## imports des préselections
etablissements_public_select_corr_dep = 

## merge ----
tableau_select_dep = data.frame(etablissements_public_select_corr_dep$beneficiaires_dep ,
                                etablissements_public_select_corr_dep$oui_non_1,
                                base_a_dep$`Code postal du bénéficaire`,
                                base_a_dep$`Date de début de l'opération`,
                                base_a_dep$`Date de fin de l'opération`,
                                base_a_dep$`Montant UE programmé`,
                                base_a_dep$`Total des dépenses éligibles`,
                                projets_select_dep
)

View(tableau_select_dep)
##export
write.csv2(tableau_select_dep, "/tableau_recapitulatif_dep.csv")
