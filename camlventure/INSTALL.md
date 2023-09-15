## Installation and Building Instructions

### Installation
<b>Creating a new OPAM Switch:</b>

(taken from Professor Clarkson's OCaml textbook at https://cs3110.github.io/textbook/chapters/preface/install.html)

To create a new OPAM Switch, run
```
opam switch create cs3110-project ocaml-base-compiler.4.14.0
```
You might be prompted to run the next command. It won’t matter whether you do or not, because of the very next step we’re going to do (i.e., logging out).
```
eval $(opam env)
```
Now we need to make sure your OCaml environment was configured correctly. Logout from your OS (or just reboot). Then re-open your terminal and run this command:
```
opam switch list
```
You should get output like this:
```
#  switch          compiler                    description
→  cs3110-project  ocaml-base-compiler.4.14.0  cs3110-project
```
Now run:
```
opam install -y utop odoc ounit2 qcheck bisect_ppx menhir ocaml-lsp-server ocamlformat ocamlformat-rpc yojson ANSITerminal Notty
```
<b>Installing libraries</b>

Please run:
```
opam install ANSITerminal
opam install Notty
```

### Building
1. Navigate to the directory called camlventure.
2. Run the following commands from the camlventure directory:
```
make build
make play
```

### How to Play
Use the arrow keys on your keyboard to move around. Your player sprite is represented by the emote 👾, and enemy trainers and camon are represented by various other emotes (they're surprises). To battle with an enemy, navigate to their location on the map, and a battle will begin.

#### Windows addtional instructions:

If you are running on a windows computer, please run the code in the Visual Studio Code terminal as the emoji sprites won't show up otherwise. Open the terminal to full screen and zoom-out as needed.
