
all: src/r0.rs src/r1.rs

src/r0.rs:	src/r0.peg
	peg src/r0.peg > src/r0.rs

src/r1.rs:	src/r1.peg
	peg src/r1.peg > src/r1.rs
