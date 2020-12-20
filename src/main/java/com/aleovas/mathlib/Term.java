package com.aleovas.mathlib;

import java.util.ArrayList;
import java.util.Collections;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


import static com.aleovas.mathlib.MiscFunctions.*;

class Term implements Comparable<Term>{
    public ArrayList<Var> vars= new ArrayList<Var>();
    public double coef;
    //Regex regCoef=new Regex("(?<!\\^)(((\\+|\\-)?[0-9.]+)|(\\+|\\-))");
    //Regex regVar=new Regex("[A-Za-z](\\^(-)?\\d+(\\.\\d+)?)?");
    Pattern regCoef=Pattern.compile("(?<!\\^)(((\\+|\\-)?[0-9.]+)|(\\+|\\-))");
    Pattern regVar=singleCharVariable?Pattern.compile("[A-Za-z](\\^(-)?\\d+(\\.\\d+)?)?"):Pattern.compile("[A-Za-z]+(\\^(-)?\\d+(\\.\\d+)?)?");
    public int id;
    public static int termID;

    public Term(String s){
        Matcher matchCoef=regCoef.matcher(s);
        Matcher matchVar=regVar.matcher(s);
        matchCoef.find();
        try{
            coef=getNumber(s.substring(matchCoef.start(),matchCoef.end()));
        }catch (IllegalStateException e){
            coef=1;
        }
        while(matchVar.find()){
            vars.add(new Var(s.substring(matchVar.start(),matchVar.end())));
        }
        clean();
        id=termID++;
    }
    public Term(double c){
        coef=c;
        id=termID++;
    }

    public String toString(){
        String temp;
        boolean mul=false;
        if(coef==1){
            temp="";
            if(vars.size()==0)temp+=" 1";
        }else if(coef==-1){
            temp="-";
            if(vars.size()==0)temp+=" 1";
        }else{
            temp= doubleToHumanReadableString(coef).replace("-","- ");
        }
        for (Var v:vars) {
            temp+=mul?mulSymbol+v.toString():v.toString();
            mul=true;
        }
        if(coef>0)temp="+ "+temp;
        if(temp.substring(0,1).equals(mulSymbol))temp=temp.substring(1);
        if(temp.substring(0,mulSymbol.length()).equals(mulSymbol))temp=temp.substring(0,mulSymbol.length());
        return temp+" ";
    }
    public String computerReadableString(){
        String temp;
        boolean mul=false;
        if(coef==1){
            temp="";
            if(vars.size()==0)temp+="1";
        }else if(coef==-1){
            temp="-";
            if(vars.size()==0)temp+="1";
        }else{
            temp= doubleToComputerReadableString(coef).replace("-","-");
        }
        for (Var v:vars) {
            temp+=mul?mulSymbol+v.sym+"^"+ doubleToComputerReadableString(v.exp):v.sym+"^"+ doubleToComputerReadableString(v.exp);
        }
        if(coef>0)temp="+ "+temp;
        if(temp.substring(0,1)==mulSymbol)temp=temp.substring(1);
        if(temp.substring(temp.length()-mulSymbol.length()).equals(mulSymbol))temp=temp.substring(0,temp.length()-mulSymbol.length());
        return temp;
    }
    public boolean equals(Term t){
        return varString().equals(t.varString());
    }
    public String varString(){
        String temp="";
        for(Var v:vars) temp+=v.sym+"^"+v.exp;
        return temp;
    }
    public void clean() {
        ArrayList<Var> temp=new ArrayList<Var>();
        //merges duplicate variables and adds their exponents
        for (Var v:vars){
            boolean notFound=true;
            for(Var x:temp){
                if(v.id!=x.id&&v.sym.equals(x.sym)){
                    x.exp+=v.exp;
                    notFound=false;
                }
            }
            if(notFound)temp.add(v);
        }
        vars.clear();
        for(Var v:temp)vars.add(v);
        temp.clear();
        //Remove variables with the power of zero
        for(Var v:vars)if(v.exp==0)temp.add(v);
        for(Var v:temp)vars.remove(v);
        Collections.sort(vars);
    }
    public Term copy(){
        Term t=new Term(coef);
        for(Var v:vars)t.vars.add(v.copy());
        return t;
    }
    public double getDegree(){
        double d=0;
        for(Var v:vars)d+=v.exp;
        return d;
    }
    public double getDegree(String sym){
        double d=0;
        for(Var v:vars)if(v.sym.equals(sym))d+=v.exp;
        return d;
    }

    public int compareTo(Term term) {
        if(term==null)return 1;else return -Double.compare(this.getDegree(),term.getDegree());
    }

    public Term mul(Term t){
        Term temp1=this.copy();
        Term temp2=t.copy();
        temp1.coef*=temp2.coef;
        temp1.vars.addAll(temp2.vars);
        temp1.clean();
        return temp1;
    }
    public Term mul(double d){
        Term temp=this.copy();
        temp.coef*=d;
        temp.clean();
        return temp;
    }
    public Term inverse(){
        if(coef==0)throw new DivideByZeroException("You don't divide by zero");
        Term temp=this.copy();
        temp.coef=1/temp.coef;
        for (Var v:temp.vars)v.exp*=-1;
        return temp;
    }
    public Term div(Term t){return this.mul(t.inverse());}
    public Term substitute(String s, double d){
        Term temp=this.copy();
        for (Var v:temp.vars) if(v.sym.equals(s)){
            temp.coef*=Math.pow(d,v.exp);
            v.exp=0;
        }
        temp.clean();
        return temp;
    }
    public Term substitute(String s, Term t){
        Term temp=this.copy();
        ArrayList<Var> tempVars=new ArrayList<Var>(); //To avoid exception from modifying vars while iterating
        for (Var v:temp.vars) if(v.sym.equals(s)){
            temp.coef*=Math.pow(t.coef,v.exp);
            tempVars.addAll(t.pow(v.exp).vars);
            v.exp=0;
        }
        temp.vars.addAll(tempVars);
        temp.clean();
        return temp;
    }
    public Polynomial subtitute(String s,Polynomial p){
        Polynomial temp=new Polynomial(0);
        for(Var v:vars)if(v.sym.equals(s)){
            if(((int)v.exp)!=v.exp)throw new PolynomialFloatPowerException();
            temp=temp.add(p.pow((int)v.exp).mul(substitute("s",1)));
        }
        temp.clean();
        return temp;
    }
    public Term pow(double d){
        Term temp=this.copy();
        temp.coef= Math.pow(temp.coef,d);
        for(Var v:temp.vars)v.exp*=d;
        temp.clean();
        return temp;
    }


}
