progr       = {exp} | ({decl} ";" {progr});

decl        = (var {name} = {exp})
            | (fun {name} {args} = {exp})
            | (fun rec {name} {args}: {type} = {exp});

exp         = {atomic_exp}
            | {app_exp}
            | (if {exp} then {exp} else {exp})
            | (match {exp} with {match_exp})
            | (! {exp})
            | (- {exp})
            | (hd {exp})
            | (tl {exp})
            | (ise {exp})
            | (print {exp})
            | ({exp} "&&" {exp})
            | ({exp} "+" {exp})
            | ({exp} "-" {exp})
            | ({exp} "*" {exp})
            | ({exp} "/" {exp})
            | ({exp} "=" {exp})
            | ({exp} "!=" {exp})
            | ({exp} "<" {exp})
            | ({exp} "<=" {exp})
            | ({exp} "::" {exp})
            | ({exp} ";" {exp})
            | ({exp} "[" {nat} "]");

atomic_exp  = {const}
            | {name}
            | ("{" {progr} "}")
            | ("(" {exp} ")")
            | ("(" {comps} ")")
            | (fn {args} "=>" {exp} end);

app_exp     = ({atomic_exp} {atomic_exp})
            | ({app_exp} {atomic_exp});

const       = "true" | "false" | {nat} | "()" | ("(" {type} "[])");

comps       = ({exp} , {exp})
            | ({exp} , {comps});

match_exp   = "end"
            | ("|" {cond_exp} "->" {exp} {match_exp});

cond_exp    = {exp}
            | "_";

args        = "()"
            | ("(" {params} ")");

params      = {typed_var}
            | ({typed_var} , {params});

typed_var   = {type} {name};

type        = {atomic_type}
            | ("(" {types} ")")
            | ("[" {type} "]")
            | ({type} "->" {type});

atomic_type = "Nil"
            | "Bool"
            | "Int"
            | ("(" {type} ")");

types       = ({type} , {type})
            | ({type} , {types});
