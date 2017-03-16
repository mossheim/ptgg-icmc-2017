%{
Welcome to LilyPond
===================

Congratulations, LilyPond has been installed successfully.

Now to take it for the first test run.

  1. Save this file

  2. Select

       Compile > Typeset file

  from the menu.

  The file is processed, and

  3.  The PDF viewer will pop up. Click one of the noteheads.


That's it.  For more information, visit http://lilypond.org .

%}


\new RhythmicStaff {
	\time 3/4
	a8 a a a a4 | a4 a a8. a16 | a8 a a8. a16 a4 | \times 2/3 {a8 a a} a4 a  | a2. |
}

%{

\new RhythmicStaff {
	\time 3/4
	a8 a8 a8. a16 a8. a16 | a8 a8 a8. a16 a8. a16 | a4 a \times 2/3 {a8 a a} | a8 a8 \times 2/3 {a8 a a} a8. a16 | a2.
}

\new RhythmicStaff {
	\time 3/4
	a4 a a8 a | a a a a a a16 a | a4 a a8. a16 | a4 a a8. a16 | a2.
}

\new RhythmicStaff {
	\time 3/4
	a4 a a8 a | a4 a a8 a | a8 a16 a16 a8 a16 a a4 | a8 a16 a16 a8 a16 a a4 | 
	a4 a a | a2 a8. a16 | a4 a a | a2 a8. a16 | a2.
}

\new RhythmicStaff {
	\time 3/4
	a4 \times 2/3 { a8 a a a a a } | a2 \times 2/3 { a8 a a } | a4 a8 a a4 | a4 a8 a a4 | a4 \times 2/3 { a8 a a a a a } | a2 \times 2/3 { a8 a a } | a4 a8 a a4 | a4 a8 a a4 | a2.
}

%}

\version "2.19.54"  % necessary for upgrading to future LilyPond versions.
