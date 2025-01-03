//! Core parsing and primary API

pub mod structs;

pub mod fns;

pub mod cmds;

pub mod errors;

pub mod conv;

use std::collections::HashMap;
use std::sync::RwLock;
use lazy_static::lazy_static;

lazy_static! {
	pub(crate) static ref RE_CACHE: structs::RegexCache = structs::RegexCache(RwLock::new(HashMap::new()));
}

enum CmdType {
	///monadic pure function
	Fn1(fns::Mon),

	///dyadic pure function
	Fn2(fns::Dya),
	
	///triadic pure function
	Fn3(fns::Tri),

	///impure command
	Cmd(cmds::Cmd),

	///impure command with register access
	CmdR(cmds::CmdR),

	///`exec`-specific (macros, IO, OS...)
	Special,

	///begin value literal
	Value,

	///no command
	Space,

	///invalid command
	Wrong,

	///not yet assigned, TODO: remove
	Temp
}

/// Direct mapping of ASCII to commands/functions, mostly a jump table
/// 
/// PHFs are for non-schizophrenics
const CMDS: [CmdType; 128] = {
	use CmdType::*;
	use fns::*;
	use cmds::*;
	[
		//NUL		SOH			STX			ETX			EOT			ENQ			ACK			BEL			BS			HT			LF			VT			FF			CR			SO			SI
		Space,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Space,		Space,		Wrong,		Wrong,		Space,		Wrong,		Wrong,

		//DLE		DC1			DC2			DC3			DC4			NAK			SYN			ETB			CAN			EM			SUB			ESC			FS			GS			RS			US
		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,		Wrong,

		//SP		!			"			#			$			%			&			'			(			)			*			+			,			-			.			/
		Space,		Fn1(inv),	Temp,		Special,	Temp,		Fn2(ph2),	Temp,		Value,		Value,		Wrong,		Fn2(mul),	Fn2(add),	Cmd(phc),	Fn2(sub),	Special,	Fn2(div),

		//0			1			2			3			4			5			6			7			8			9			:			;			<			=			>			?
		Value,		Value,		Value,		Value,		Value,		Value,		Value,		Value,		Value,		Value,		Special,	Cmd(phc),	Fn2(ph2),	Fn2(ph2),	Fn2(ph2),	Special,

		//@			A			B			C			D			E			F			G			H			I			J			K			L			M			N			O
		Value,		Temp,		Temp,		Cmd(phc),	Cmd(phc),	Temp,		Value,		Temp,		Temp,		Cmd(phc),	Temp,		Cmd(phc),	CmdR(phr),	Temp,		Temp,		Cmd(phc),

		//P			Q			R			S			T			U			V			W			X			Y			Z			[			\			]			^			_
		Special,	Special,	Cmd(phc),	CmdR(phr),	Value,		Temp,		Temp,		Temp,		Special,	Temp,		Temp,		Value,		Special,	Wrong,		Fn2(pow),	Temp,

		//`			a			b			c			d			e			f			g			h			i			j			k			l			m			n			o
		Special,	Temp,		Temp,		Cmd(phc),	Cmd(phc),	Temp,		Temp,		Temp,		Temp,		Cmd(phc),	Temp,		Cmd(phc),	CmdR(phr),	Temp,		Temp,		Cmd(phc),

		//p			q			r			s			t			u			v			w			x			y			z			{			|			}			~			DEL
		Special,	Special,	Cmd(phc),	CmdR(phr),	Temp,		Temp,		Temp,		Temp,		Special,	Temp,		Temp,		Temp,		Fn3(ph3),	Temp,		Fn2(ph2),	Wrong
	]
};