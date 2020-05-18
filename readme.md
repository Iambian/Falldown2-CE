Falldown CE
===========

Warning
-------
* This game works on a **TI-84 Plus CE**
* This game **will not** work on a TI-84 Plus (SE). Go play
	[this](https://www.ticalc.org/archives/files/fileinfo/140/14064.html)
	if you have that calculator.
* This game **will not** work on a TI-84 CSE. If you have that calculator,
	you're out of luck.

About
-----
An update/recreation of [this](https://github.com/Iambian/Falldown-CE)
game written completely in assembler. As such the stuff afterwards is pretty
much going to be a copy of the original.


Move a ball left and right to keep it falling down through the gaps in
the ever-rising floors. Don't touch the top of the screen for as long
as possible and post the highest score!

Installing and Running
----------------------

Use your favorite computer to calculator link software (TiLP, TIConnect, etc)
to send FALLDOWN.8xp to your calculator.

To run without a shell, follow these steps:
1. Turn on the calculator and make sure you're on the home screen.
2. Then push the following keys in order:
   [CLEAR] [2nd] [0] [DOWN] [DOWN] [DOWN] [DOWN] [DOWN] [DOWN] [ENTER]
3. Push [PRGM] then move the cursor down until FALLDOWN is selected.
   When you have done that, push [ENTER]
4. Your homescreen should look something like:

   `Asm(prgmFALLDOWN)`

   Then push [ENTER] to start the game.

Controls
--------

| Key          | Function in menu
| ------------:|:----------------
| [2nd]        | Select menu option
| [Up], [Down] | Change menu option
| [Mode]       | Quit the game


| Key(s)         | Function in game
| --------------:|:----------------
| [Left],[Right] | Move the ball left or right
| [Mode]         | Return to the main menu



Troubleshooting
---------------
1. * Q: When I run the game, the calculator gives me `ERROR:ARCHIVED`
   * A: Follow the grey text prompt to unachive the variable.

2. * Q: I think I found a bug. Where can I report it?
   * A: You can report it here: http://cemete.ch/p264415

License
-------
I use the MIT license for this project. See `LICENSE` for more details.

Version History
---------------

* 0.0 - Initial commit.
* 0.1 - Game mostly works.
		Known problem: Tearing on bottom corner when played on hardware
* 0.2 - Rewritten in assembler. Uses a different (incompatible)
		score save file.







