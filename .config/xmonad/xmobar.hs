Config {
    position = TopP 0 0,
    --font = "xft:FiraCode:Style=Retina:size=10",
    font = "xft:iosevka:Style=bold:size=10",
    bgColor = "#222222",
    fgColor = "#DDDDDD",
    lowerOnStart = False,
    overrideRedirect = False,
    allDesktops = False,
    persistent = True,
    commands = [
        Run StdinReader,
	    Run Date "%a %b %_d %l:%M" "date" 10,
        Run Memory ["-t","Mem: <usedratio>%","-H","8192","-L","4096","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run Swap ["-t","Swap: <usedratio>%","-H","1024","-L","512","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC"] 10,
        Run MultiCpu ["-t","Cpu:<total0><total1><total2><total3><total4><total5><total6><total7>\
                            \<total8><total9><total10><total11><total12><total13><total14><total15>",
                            "-L","30","-H","60","-h","#FFB6B0","-l","#CEFFAC","-n","#FFFFCC","-w","3"] 10
    ],
    sepChar = "%",
    alignSep = "}{",
    template = " <fc=#FFFFCC>%StdinReader%</fc> } <fc=#FFFFCC>%date%</fc> { %multicpu%   %memory% "
}
