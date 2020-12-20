package com.aleovas.mathlib;

import static com.aleovas.mathlib.MiscFunctions.*;

//A simple data container containing exponents and symbols for variables
public class Var implements Comparable<Var>{
    double exp;
    String sym;
    int id; //id is used for comparison during manipulation of Terms to prevent recursion
    private static int varID; //varID is incremented every time a new variable is created to keep track of variables
    public Var(String s){
        //Parses a string (usually passed from the Term class) to get the symbol and exponent
        String temp = "";
        if(s.contains("^")){
            temp=s.substring(s.indexOf("^"));
            exp=Double.parseDouble(temp.substring(1));
            s=s.replace(temp,"");

        }else{
            exp=1;
        }
        sym=s;
        id=varID++;
    }
    public Var(double exp, String sym){
        this.exp=exp;
        this.sym=sym;
        id=varID++;
    }
    public String toString(){
        if(isNumeric(sym)) return sym+getExponent(exp)+mulSymbol;
        return sym+getExponent(exp);
    }
    public Var copy(){
        return new Var(exp,sym);
    }
    @Override
    public int compareTo(Var var) {
        //Comparison is used for sorting, variables are sorted in alphabetical order
        if(var==null)return 1;
        return this.sym.compareTo(var.sym);
    }
}
