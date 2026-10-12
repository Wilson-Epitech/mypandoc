<div align="center">

# MyPandoc

**Un convertisseur de documents en Haskell, avec sa propre bibliothèque de parsing.**

<img alt="Haskell" src="https://img.shields.io/badge/Haskell-5D4F85?style=for-the-badge&logo=haskell&logoColor=white"/> <img alt="Stack" src="https://img.shields.io/badge/Stack-5D4F85?style=for-the-badge"/>

<sub>Projet à 4 · 2024</sub>

</div>

<br/>

<p align="center"><img src=".github/assets/term-mypandoc.png" alt="MyPandoc" width="820"/></p>

## Le projet en quelques mots

Une version simplifiée de *pandoc* : on lit un document en Markdown, XML ou JSON, et on le réécrit dans un autre format. Sans Parsec, le parsing repose sur nos propres combinateurs.

## Ce que ça fait

- Bibliothèque de **parser combinators** écrite à la main
- Représentation commune : en-tête, sections, paragraphes, listes, gras, italique, code
- Conversions Markdown → XML et Markdown → JSON
- Choix du format de sortie (`-f`) et du fichier de sortie (`-o`)

## Lancer le projet

```bash
make
./mypandoc -i examples/doc.md -f xml
./mypandoc -i examples/doc.md -f json
```

## L'équipe

<a href="https://github.com/Wilson-Epitech/mypandoc/graphs/contributors"><img src="https://contrib.rocks/image?repo=Wilson-Epitech/mypandoc" alt="Contributeurs"/></a>

Edward Lalande, Timothée Plisson, Paul Bardeur, Wilson Bordichon

**Ma partition :** J'ai écrit la conversion **Markdown → XML** : en-tête, sections, paragraphes, gras, italique, code, blocs de code et listes.
