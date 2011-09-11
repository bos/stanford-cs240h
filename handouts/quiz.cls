% -*-latex-*-

% quiz.cls by David Mazieres; public domain
%
% \documentclass[scpd,answers]{quiz}
%
% Defines several macros for exams.
%       scpd - places SCPD checkbox on cover page
%
% You must start the exam with:
%   \minutes{180} % # of minutes exam lasts
%   \title{Class \\ Exam description -- date}
%
% You can optionally include in the preamble extra advice for the
% cover page, for example:
%
%   \advice{Some problems are much harder than others.
%
%           We will take off points for overly verbose answers.}
%
% Define doanswers to print the answers.  It's convenient to create a
% two-line file (say quiz-answers.tex) that includes the quiz like
% this:
%   \def\doanswers{}
%   \input{quiz.tex}
%
% Each problem should be enclosed in a prob environment:
%
% \begin{prob}{#points}
% ...
% \end{prob}
%
% Multiple choice questions should be enclosed in an mchoice,
% mchoice1, mchoice0, or mchoice* environment:
%
% \begin{mchoice}
% \item First answer
% \item Second answer
% \citem Correct answer, which should be circled in solution
% \item Fourth answer
% ...
% \end{mchoice}
%
% Use mchoice1 for questions with only one answer, mchoice* for
% questions where one or more answers may need to be circled, mchoice0
% for questions where 0 or more answers may need to be circled, and
% just plain mchoice if you want to add your own text before the
% question.
%
% Space for answers should be specified in an answer environment.  For
% example:
%
% \begin{answer}{2in}
%
% Actual text of the answer can go in here, for the answer key.
%
% \end{answer}
%
% If answers need to be spread throughout the definition of a table,
% include any answer text in the macro \anstxt{...}.
%
%

% Identification

\ProvidesClass{quiz}[2009/03/13]

% Initial code

\newif\ifscpd

% Options

\DeclareOption{scpd}{\scpdtrue}
\DeclareOption{answers}{\def\doanswers{}}
\DeclareOption{10pt}{\OptionNotUsed}
\DeclareOption{11pt}{\OptionNotUsed}
\DeclareOption{12pt}{\OptionNotUsed}
\DeclareOption{onesided}{\special{! statusdict /setduplexmode known
    { statusdict begin false setduplexmode end } if}}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}

\ProcessOptions

% Package loading

\LoadClass[12pt]{article}
\RequirePackage{color}
\RequirePackage{fullpage}

% Code

\makeatletter

\newif\ifans
\@ifundefined{doanswers}{\ansfalse}{\anstrue}

\advance\footskip by 23 true pt\relax
\parindent=0pt
\parskip=1ex

\def\@oddfoot{\hfill Page~\arabic{page} of \pageref{lastprob}}
\renewcommand{\thesection}{\Roman{section}}
\@ifundefined{orig@section}{\let\orig@section=\section}\relax
\renewcommand{\section}{\newpage\orig@section}

\newcounter{pctr}
\stepcounter{pctr}
\newcounter{points}
\def\@newscores{}
\newcommand{\probglue}{\vskip 0pt plus.3\textheight}

\newenvironment{prob}[1]%
  {\addtocounter{points}{#1}%
    {\toks0=\expandafter{\@newscores}%
      \xdef\@newscores{\the\toks0 \noexpand\do{\thepctr}{#1}}}%
   \protected@edef\@currentlabel{\thepctr}%
   \penalty-500
   \setbox0=\hbox{\textbf{88.~~}}
   \noindent\hbox to\wd0{\hss\textbf{\thepctr.~~}}%
   \textbf{[#1~points]:}\stepcounter{pctr}
   \vspace{-\topsep}
   \list{}{\leftmargin=\wd0}\item\relax}%
  {\endlist\nobreak\probglue}
\newcommand\probnote[1]%
  {\\\begin{tabular}{cr} \hspace{3in} & {\bf (#1)} \\ \end{tabular}}

\def\@nsfont{\sffamily\slshape}
\def\@nscolor{\color[rgb]{.9,0,0}}
\ifans
  \newcommand{\do@nswer}[1]{#1}
\else
  \newcommand{\do@nswer}[1]{}
\fi
\DeclareRobustCommand{\anstxt}[1]%
    {\do@nswer{\color@setgroup\@nscolor\@nsfont #1\color@endgroup}}

% Consumes as much space as the answer
\DeclareRobustCommand*{\ansbox}[1]%
    {\setbox0=\hbox{\color@setgroup\@nscolor\@nsfont\LARGE\strut
        #1\color@endgroup}%
     \ifans\unhbox0%
     \else\hbox to\wd0{\vrule height\ht0 depth\dp0 width0pt\hfil}\fi}

\ifans
  \let\@gobbleans=\relax
\else
  \def\@nswer{answer}
  \long\def\@gobbleans#1\end#2{\bgroup\def\tmp{#2}
    \ifx\tmp\@nswer
      \edef\donext{\noexpand\end{\tmp}}
    \else
      \let\donext=\@gobbleans
    \fi\expandafter\egroup\donext}
  % Note \expandafter because \donext becomes undefined after \egroup
\fi

\newenvironment{answer}[1]%
  {\nobreak\vskip-\lastskip\begingroup\set@color
    \def\@nsspace{#1}
    \vbox \ifx\@nsspace\@empty\else to #1\fi\bgroup
    \@gobbleans\@nscolor\@nsfont\textbf{Answer:}\par}%
  {\vfil\null\egroup\endgroup\probglue}

\newif\ifcitem
\newcommand{\putmccnt}[1]{
\color@begingroup
\begin{picture}(10,5)
\put(0,0){\textbf{#1}}
\ifcitem
\put(5,4){\@nscolor\circle{15}}
\put(5,4){\@nscolor\circle{16}}
\fi
\end{picture}
\global\citemfalse
\color@endgroup
}
\newcommand{\citem}{\do@nswer{\citemtrue}\item}
% Use \citem for correct (circled) answers to multiple choice
% questions, and \item for incorrect answers

\newcounter{mccnt}
\newenvironment{mchoice}%
  {\list{\putmccnt{\Alph{mccnt}}}{\advance\itemsep by .5\baselineskip
      \usecounter{mccnt}}}%
  {\endlist}
\newenvironment{mchoice1}%
  {\par\textbf{Circle the best answer.} \begin{mchoice}}%
  {\end{mchoice}}
\newenvironment{mchoice0}%
  {\par\textbf{Circle all that apply.  (There may be zero, one, or multiple
      answers.)}\begin{mchoice}}%
  {\end{mchoice}}
\newenvironment{mchoice*}%
  {\par\textbf{Circle all that apply.  (There may be multiple
      answers.)}\begin{mchoice}}%
  {\end{mchoice}}

\def\my@minutes{180}
\newcommand*{\minutes}[1]{\def\my@minutes{#1}}

\def\my@advice{%
% Some questions may be much harder than others.
%
% All questions require you to justify your answer to receive full
% credit, even multiple choice questions for which you circle the
% correct answer(s).
%
% Keep your answers concise.  We will deduct points for a correct answer
% that also includes incorrect or irrelevant information.
}
\newcommand{\advice}[1]{\def\my@advice{#1}}

\def\if@ne#1#2#3{\ifnum #1=1\relax #2\else #3\fi}
\def\r@wref#1{\@ifundefined{r@#1}{0}%
  {\expandafter\expandafter\expandafter\@firstoftwo\csname r@#1\endcsname}}
\def\ifref@ne#1#2#3{\if@ne{\r@wref{#1}}{#2}{#3}}

\renewcommand{\maketitle}{
\newpage\null

\vskip 0pt plus .5fill

{\Large\bfseries\centering\relax\@title}

\vskip.5in plus .5fill

\begingroup
\hsize 5 true in

{\Large\bfseries
\begin{tabular}{rl}
Your Name: & \underline{\hbox to3.5 true in{%
    \anstxt{\qquad Answers}\hfil}}\\[.2in]
%
SUNet ID: & \underline{\hbox to3.5 true in{%
    \anstxt{\qquad \doanswers}\hfil\texttt{@stanford.edu}}}\\[.2in]
%
\noalign{\raggedright\large
\mbox{\mdseries\strut\ifscpd
  Check if you would like the exam returned to you via SCPD: \ 
  {\fboxsep=0pt\fbox{\vrule width 0pt height 11pt depth
      0pt\hbox to 10pt{\hss}}}\fi}

\endgraf\kern.2in

In accordance with both the letter and the spirit of the Stanford
Honor Code, I neither received nor provided any assistance on this
exam. \vspace{\baselineskip}}
%
Signature: & \underline{\hbox to3.5 true in{\hfil}}\\
\end{tabular}}

\vskip.5in plus 1fill

{\raggedright\large\mdseries
  \parskip=\itemsep\advance\parskip by\parsep
  \everypar={\hangindent=\leftmargin
    \hbox to\labelwidth{\hfil$\bullet$}\hspace*{\labelsep}}

The exam has \ref{lastprob} question\ifref@ne{lastprob}{}{s} totaling
\ref{totpoints} point\ifref@ne{totpoints}{}{s}.

You have \my@minutes~minute\if@ne\my@minutes{}{s} to complete
\ifref@ne{lastprob}{it}{them}.

\my@advice
\par}

\nointerlineskip

\moveright 5.5 true in \vbox to0pt {\vss
  \large
  \@ifundefined{scores}\relax{

    \def\fmtprob##1{\textbf{\ ##1\unskip}}
    \dimendef\maxscore=9 \maxscore=0pt
    {\def\do##1##2{\setbox0=\hbox{\fmtprob{##1}}
        \ifdim\wd0 > \maxscore\relax \global\maxscore=\wd0\fi}
      \scores}

    {\def\do##1##2{\relax &\fmtprob{##1}&##2\unskip\cr
                   &\multispan2\hrulefill\cr}
      \offinterlineskip
      \halign{\hfill##%
        & \vrule\hbox to\maxscore{\hfill##}
        & \vrule \vrule width0pt height18pt \strut
          \normalsize \hskip 36pt /##\hfil\,\vrule\cr

        &\multispan2\hrulefill\cr
        \scores
        \noalign{\vskip 2pt}
        \hline
        \vrule height 20pt\bfseries\Large \ Total\hskip-\maxscore&\omit
           & \normalsize\ref{totpoints}\cr
        \hline
      }
    }
  }}
\endgroup\newpage}
\AtBeginDocument\maketitle


\newcommand{\@ndex@m}{%
   \addtocounter{page}{-1}%
   \addtocounter{pctr}{-1}%
   \immediate\write\@auxout{\string
   \newlabel{lastprob}{{\thepctr}{\thepage}}}%
   \addtocounter{pctr}{1}%
   \immediate\write\@auxout{\string
   \newlabel{totpoints}{{\thepoints}{\thepage}}}%
   \addtocounter{page}{1}%
   \toks0=\expandafter{\@newscores}%
     \immediate\write\@auxout{\string\gdef\string\scores{\the\toks0}}%
   \ifx\scores\@newscores\else%
     \message{LaTeX Warning: Scores have changed; Rerun to make cover page}\fi%
}
\AtEndDocument{\clearpage\@ndex@m}

\makeatother

%
% Misc macros
%
\RequirePackage{listings}
\lstnewenvironment{code}[1][]%
    {\lstset{language=C,escapeinside=`',%
            columns=flexible,keepspaces=true,%
            commentstyle=,%
            basicstyle=\ttfamily,%
            #1}}%
    {}
