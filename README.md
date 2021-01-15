# Projet Satsolver Haskell
## Installer la plateforme Haskell
```bash
sudo apt-get install haskell-platform
curl -sSL https://get.haskellstack.org/ | sh
```
## Récupérer notre Satsolver Haskell
```bash
git clone https://github.com/Tsugaryu/Haskell-Pr.git
```
## Important commands to use the project
- stack build : Compiler project
- stack haddock : Creer documentation
- stack exec -- ghci : rendre les executables accessibles
- stack test : générer les test

## Commentaires divers
- Nous avons réalisé des tests unitaires que vous pourrez lancer en exécutant la commande `stack test` comme précisé précédemment. pour cela nous avons utilisé les bibliothèques `Test.Tasty` et `Test.Tasty.HUnit`
- Dans le fichier Fml, vous pourrez trouver une ébauche commentée de Simplify
