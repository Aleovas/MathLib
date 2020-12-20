package com.aleovas.mathlib;

import static com.aleovas.mathlib.MiscFunctions.*;

public class Var implements Comparable<Var>{
    double exp;
    String sym;
    int id;
    static int varID;
    public Var(String s){
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
        if(var==null)return 1;
        return this.sym.compareTo(var.sym);
    }
}
