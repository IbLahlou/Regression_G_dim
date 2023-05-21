# Project Name

PARTICIPATION AU PROJET DE MODÉLISATION (COMPÉTITION DE MODÈLES)

## Table of Contents

- [Introduction](#introduction)
- [Features](#features)
- [Installation](#installation)
- [Objective](#objective)
- [Problematique](#problems)

## Introduction

Les données d'une étude sur le cancer de la prostate (Stamey et al., 1989):

## Features

Les variables sont les mesures cliniques suivante :
Les variable prédictives : 
- log(volume du cancer) (lcavol)
- log(poids de la prostate) (lweight)
- âge (age), 
- Le logarithme de la quantité d'hyperplasie bénigne de la prostate (lbph)
- Invasion des vésicules séminales (svi)
- log(pénétration capsulaire) (lcp)
- Score de Gleason (gleason)
- Pourcentage score de Gleason 4 ou 5 (pgg45).

La variable réponse est le logarithme de l'antigène spécifique de la prostate (lpsa). 
La variable "train" contient les spécification du training et du test set.


## Installation

Librairie utilisé :
- Heatmaply
- quadprog
- ggplot2

## Objective

Comparaison entre les modèle (OLS, Ridge, Lasso, Elastic net et Régression en composantes principales) en
calculant leur erreur quadratique moyenne de prédiction sur les données de test, ainsi que le nombre
et les noms des variables sélectionnées par chaque modèle.

## Problematique

- Echec à l'utilisation de la méthode LOOCV pour regression ridge et elastic net pour la recherche des meilleurs lambda (j'ai opté pour le grid search)

## Contact

- Email : ibrahim.lahlou@ump.ac.ma

