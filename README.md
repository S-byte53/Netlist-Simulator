# Netlist Simulator - Projet pour le cours "Systèmes numériques"

Projet en OCaml qui a pour but de simuler des Netlists, données par des fichiers *.net*

[**Lien vers le dépôt GitHub**](https://github.com/S-byte53/Netlist-Simulator.git)

## Compilation

Pour **compiler** il existe plusieurs façons equivalentes:

```bash
make
```

```bash
make build
```

On pourra aussi regarder le contenu du makefile si l'on souhaite, pour exécuter les commandes individuelles.

> Après la compilation, un fichier "netlist_simulator.byte" sera crée.

## Exécution

Pour exécuter, voici deux façons de faire:

```bash
make run ARGS="..." 
```

```bash
#supposant que vous vous trouvez à la racine du projet
./tp1/netlist_simulator.byte ...
```

Les points de suspension doivent être remplacés par des arguments, dans cet ordre:

1. -n [number]
    * indique le nombre de cycles de simulation à effectuer
    * l'argument n'est pas obligatoire --> En cas d'ommission, la simulation n'aura pas de limite sur le nombre de cycles 
        * dans ce cas, on pourra arrêter la simulation avec la commande **Ctrl+C**

2. -rom [fichier.txt]
    * indique le fichier qui fournit la mémoire ROM nécessaire pour le circuit, si c'est le cas
    * l'argument est obligatoire uniquement lorsque le circuit contient une instruction ROM
    * le fichier doit être un *.txt* qui suit le format (voir plus bas) et qui respecte les valeurs de la commande ROM du fichier *.net*
    * le repertoire **tp1** contient déjà quelques fichiers *.txt* avec des mémoires prédéfinies, suivant ou non le format (voir plus bas)

3. [fichier.net]
    * indique le fichier *.net* qui contient la description du circuit à simuler
    * on peut indiquer le nom du fichier directement, ou le chemin d'accès, selon le cas
    * quelques fichiers **.net** se trouvent dans le répertoire **tp1/test**

> Attention! Privilégiez les fichiers *.net* qui sont soit dans le répertoire **tp1**, soit dans un de ses sous-répertoires (ex. **test**). Si vous exécutez avec la commande `make run ARGS="..."`, l'exécution sera faite depuis **tp1**. Surtout si vous exécutez avec la commande manuelle, il est important de faire attention au chemin du fichier.


## Elimination des fichiers qui ne sont pas nécessaires

Utilisez la commande:

```bash
make clean
```

> Ceci va éliminer tous les fichiers *.byte*, *.cmx*, etc., qui ont été crées lors de la compilation du projet. 

## Utilisation des fonctionnalités ROM

Si le circuit à tester ne contient pas d'instruction ROM, l'argument *-rom* n'est pas nécessaire lors de l'exécution.

Sinon, si le circuit contient une instruction ROM, il faut fournir un fichier *.txt* qui suit le format suivant:

```
2 4
0011
1101
0101
1111
```
* La première ligne **doit contenir** 2 entiers, la *addr_size*, en nombre de bits, et la *word_size*, en nombre de bits.
* Les valeurs introduites doivent correspondre aux arguments de l'instruction ROM du circuit. Une exception sera levée sinon.
* Il faut introduire au moins `2^addr_size` lignes supplémentaires, chacune contenant une chaîne de caractères `0` ou `1`, de longueur *word_size*.

> Attention! Il est important que les lignes `1 -> 2^addr_size` respectent le format! Tout caractère qui n'est pas un `0` ou `1`, ou toute chaîne de longueur qui n'est pas égale à `word_size` provoquera une exception

* On peut avoir des lignes qui suivent celles de la vrai mémoire ROM, et ces lignes n'ont pas besoin de respecter le format. Par exemple:

```
2 4
0011
1101
0101
1111
0101 --> ignoré
1010 --> ignoré
les dernières lignes n'ont pas besoin de respecter le format
```

* Des fichiers *.txt* sont fournis dans le répertoire **tp1**, notamment **ROM_OK.txt**, qui peut être utilisé pour tester le fichier **rom.net**.

## Utilisation du simulateur

Pour utiliser le simulateur, il suffit de compiler et exécuter, avec les arguments souhaités. Sur la ligne de commande on verra le numéro du cycle en cours et des affichages indiquant la variable qu'on est en train d'introduire, et le nombre de bits attendus. Après la lecture, on verra les variables qui sont attendues en sortie et leurs valeurs.

> Attention! Il est important de respecter le nombre de bits demandé, et de saisir des chaînes de caractères contenant uniquement des `0` ou `1`.  Toute saisie incorrecte va lever une exception.

> Attention! Le bit de poids fort se trouve **à droite**, et le bit de poids faible se trouve **à gauche**!

Exemples d'utilisation:

```
Step 1:
a  -> 1 bit(s) ? 0
b  -> 1 bit(s) ? 0 
c  -> 1 bit(s) ? 0
  => r = 0
  => out_c = 0
Step 2:
a  -> 1 bit(s) ? 1
b  -> 1 bit(s) ? 1
c  -> 1 bit(s) ? 0
  => r = 0
  => out_c = 1
```

```
Step 1:
read_addr  -> 2 bit(s) ? 00
  => o = 0110
Step 2:
read_addr  -> 2 bit(s) ? 01
  => o = 1111
```

>Attention! L'adresse `01` sera interprétée comme un *2*, et non pas un *1*, en décimal!