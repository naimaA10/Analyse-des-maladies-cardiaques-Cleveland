# Analyse des maladies cardiaques - Cleveland

## Description

Ce projet analyse les données de la Cleveland Heart Disease Database pour étudier les facteurs de risque associés aux maladies cardiaques. Les données incluent des informations sur l'âge, le sexe, le cholestérol, la fréquence cardiaque maximale à l'effort, les types de douleurs thoraciques, et d'autres tests diagnostiques. L'objectif est d'identifier les caractéristiques cliniques qui prédisent la présence de maladies cardiaques.

## Objectifs

- Identifier les facteurs de risque des maladies cardiovasculaires.
- Analyser l'influence de l'âge, du sexe, du cholestérol et des tests d'effort sur la santé cardiaque.
- Appliquer des tests statistiques pour comparer les groupes et déterminer les différences significatives.

## Variables

Les variables présentes dans l'ensemble de données sont :

- `âge` : Âge du patient.
- `sexe` : Sexe du patient (1 = Homme, 0 = Femme).
- `type_douleur` : Type de douleur thoracique (4 modalités).
- `pression_arterielle` : Pression artérielle au repos.
- `cholesterol` : Taux de cholestérol sanguin.
- `sucre` : Niveau de sucre dans le sang (mg/dl).
- `resting_electrocardiographic` : Résultats de l'électrocardiogramme au repos.
- `freq_max` : Fréquence cardiaque maximale atteinte lors du test d'effort.
- `angine_exercice` : Indication de l'angine de poitrine lors de l'exercice (1 = Oui, 0 = Non).
- `ST_depression_exercise` : Dépression du segment ST pendant l'exercice.
- `target` : Cible de la maladie cardiaque (1 = Malade, 0 = Sain).

## Analyse réalisée

Les analyses réalisées incluent :

- **Statistiques descriptives** pour explorer les données (moyennes, médianes, etc.).
- **Tests de normalité** pour vérifier la distribution des données.
- **Comparaison de moyennes** entre les groupes (tests t, ANOVA).
- **Test de Chi²** pour tester l'indépendance entre variables catégorielles.
- **Tests non paramétriques** (comme le test de Wilcoxon) lorsque les hypothèses de normalité ne sont pas respectées.

## Résultats clés

- **Cholestérol** : Un taux de cholestérol plus élevé est un facteur de risque significatif pour les maladies cardiaques.
- **Âge** : Les patients plus âgés ont un risque plus élevé de maladies cardiaques.
- **Fréquence cardiaque maximale** : La fréquence cardiaque maximale est un indicateur clé pour prédire les symptômes des maladies cardiaques.
- **Angine à l'exercice** : La dépression du segment ST est liée à la présence d'angine à l'exercice.

## Prérequis

- R (version 4.0 ou supérieure)
- Bibliothèques R : `tidyverse`, `ggplot2`, `stats`, `dplyr`, etc.

## Auteurs

Ce projet a été réalisé par Naïma AMMICHE et Emma Soufir en 2021.