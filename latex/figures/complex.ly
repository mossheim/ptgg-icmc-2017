\layout {
	indent = #0
}

\new RhythmicStaff {
	\override TupletBracket.bracket-visibility = ##t	
	\time 15/16
	\times 2/3 {a16[ a8 \times 2/3 {a8 a16} a16]}
	\times 4/5 {a8.[ a8]}
	\times 2/3 {\times 4/5 {a16[ a a a a} \times 2/3 {a8 a16]} }
	a16 a a |
	
	\time 5/8
	a8 a a16 a \times 4/5 {a8. a8} | \break
	
	\time 7/8
	\times 4/5 {a16 a a a a} \times 4/5 {a8 a16 a a} a8[ a a] | 

	\time 15/16
	a8[ a8.]
	\times 2/3 {a8[ a16} a8.]
	a8.[ a8]

	\time 3/4
	a2.
}

\version "2.19.54"  % necessary for upgrading to future LilyPond versions.
