%option stack 8bit noyywrap yylineno

%{

#include <iostream>
#include <vector>
#include <string>

static std::vector<std::string> hrefs;

/*

  <tag-qualquer attr-qualquer="ldkj" outracoisa='mjsdhfd' href=http://bla.bla.bla mais="">
  <tag-qualquer attr-qualquer="ldkj" outracoisa='mjsdhfd' HrEf = 'http://bla.bla.bla' mais="">
  <tag-qualquer attr-qualquer="ldkj" outracoisa='mjsdhfd' hREf="http://bla.bla.bla" mais="">

  <!-- href=http://nao.e.para.apanhar.este/ -->

*/

inline void yyerror(const char *msg) { std::cout << msg << std::endl; }

%}

SPACE   [ \t]
ID      [[:alpha:]]([[:alnum:]]|:|-|_)*
HREF    [Hh][Rr][Ee][Ff]{SPACE}*={SPACE}*
ATTR    {ID}{SPACE}*={SPACE}*

%x X_COMMENT X_TAG
%x X_DONTCARE X_DCA X_DCP
%x X_REF1 X_REFA X_REFP

%%

<INITIAL,X_COMMENT>"<!--"       yy_push_state(X_COMMENT);
<X_COMMENT>"-->"                yy_pop_state();
<X_COMMENT>.|\n                 ;

"<"{ID}                         yy_push_state(X_TAG);
<X_TAG>">"                      yy_pop_state();
<X_TAG>{HREF}                   yy_push_state(X_REF1);
<X_TAG>{ATTR}                   yy_push_state(X_DONTCARE);
<X_TAG>.|\n                     ;

<X_REF1>\"                      yy_push_state(X_REFA);
<X_REF1>\'                      yy_push_state(X_REFP);
<X_REF1>{SPACE}|\n              {
  yyless(yyleng-1);
  hrefs.push_back(std::string(yytext));
  yy_pop_state();
}
<X_REF1>">"                     {
  yyless(yyleng-1);
  hrefs.push_back(std::string(yytext));
  yy_pop_state();
}

<X_REFA>\"                      {
  yyless(yyleng-1);
  hrefs.push_back(std::string(yytext));
  yy_pop_state();
  yy_pop_state();
}
<X_REFP>\'                      {
  yyless(yyleng-1);
  hrefs.push_back(std::string(yytext));
  yy_pop_state();
  yy_pop_state();
}
<X_REF1,X_REFA,X_REFP>.         yymore();
<X_REFA,X_REFP>\n               yyerror("ERRO");

<X_DONTCARE>" "|\n              yy_pop_state();
<X_DONTCARE>\"                  yy_push_state(X_DCA);
<X_DONTCARE>\'                  yy_push_state(X_DCP);
<X_DONTCARE>">"                 yyless(yyleng-1); yy_pop_state();
<X_DONTCARE>.                   ;

<X_DCA>\"                       yy_pop_state(); yy_pop_state();
<X_DCP>\'                       yy_pop_state(); yy_pop_state();
<X_DCA,X_DCP>.                  ;
<X_DCA,X_DCP>\n                 yyerror("ARGH!");

.|\n                            ;

%%

int main() {
  yylex();
  for (int i = 0; i < hrefs.size(); i++)
    std::cout << "REF " << i << ": " << hrefs[i] << std::endl;
  return 0;
}
