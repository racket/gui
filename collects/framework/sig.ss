(module sig mzscheme
  (require (lib "unitsig.ss")
	   "test-sig.ss"
	   "gui-utils-sig.ss"
	   "private/sig.ss")

  (provide framework^ frameworkc^)

  (define-signature frameworkc^
    ([unit application : framework:application^]
     [unit version : framework:version^]
     [unit color-model : framework:color-model^]
     [unit exn : framework:exn^]
     [unit exit : framework:exit^]
     [unit preferences : framework:preferences^]
     [unit autosave : framework:autosave^]
     [unit handler : framework:handler^] 
     [unit keymap : framework:keymap^]
     [unit match-cache : framework:match-cache^]
     [unit paren : framework:paren^]
     [unit scheme-paren : framework:scheme-paren^]
     [unit path-utils : framework:path-utils^]
     [unit icon : framework:icon^]

     [unit editor : framework:editor^]
     [unit pasteboard : framework:pasteboard^]
     [unit text : framework:text^]

     [unit gui-utils : framework:gui-utils^]

     [unit finder : framework:finder^]

     [unit group : framework:group^]

     [unit canvas : framework:canvas^]

     [unit panel : framework:panel^]

     [unit menu : framework:menu^]
     
     [unit frame : framework:frame^]
     [unit scheme : framework:scheme^]
     [unit main : framework:main^]))

  (define-signature framework^
    ([unit test : framework:test^]
     (open frameworkc^))))