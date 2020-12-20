package com.aleovas.mathlib;

import java.util.ArrayList;

import static com.aleovas.mathlib.MiscFunctions.*;
public class Equation {
    Expression[] expressions=new Expression[2];
    public int id;
    static int eID;
    public Equation(String s){
        if(!s.contains("="))throw new InvalidEquationException();
        expressions[0]=new Expression(s.substring(0,s.indexOf("=")));
        expressions[1]=new Expression(s.substring(s.indexOf("=")+1));
        id=eID++;
    }
    public Equation(Expression left, Expression right){
        expressions[0]=left.copy();
        expressions[1]=right.copy();
        id=eID++;
    }
    public Equation(Polynomial left, Polynomial right){
        expressions[0]=new Expression(left.copy());
        expressions[1]=new Expression(right.copy());
        id=eID++;
    }
    public Equation(){
        expressions[0]=new Expression();
        expressions[1]=new Expression();
        id=eID++;
    }
    public void clean(){
        expressions[0].clean();expressions[1].clean();

    }
    public Equation copy(){
        return new Equation(expressions[0],expressions[1]);
    }
    public int getVarCount(){
        return getVars().size();
    }
    public ArrayList<String> getVars(){
        ArrayList<String> temp=expressions[0].getVars();
        for(String s:expressions[1].getVars())if(!temp.contains(s))temp.add(s);
        return temp;
    }

    public Equation subtitute(String sym, Polynomial p){
        Equation e=copy();
        e.expressions[0]=e.expressions[0].subtitute(sym, p);
        e.expressions[1]=e.expressions[1].subtitute(sym, p);
        clean();
        return e;
    }
    public Equation subtitute(String sym, double d) {
        Equation e=copy();
        e.expressions[0]=e.expressions[0].subtitute(sym, d);
        e.expressions[1]=e.expressions[1].subtitute(sym, d);
        clean();
        return e;
    }
    public void solve(){

    }
    public void solveSystem(Equation e){

    }
    public void solveSystem(Equation e1, Equation e2){

    }
    public Equation rearrange(String var){
        Equation temp=copy();
        Expression left=temp.expressions[0];Expression right=temp.expressions[1];
        for(FunctionGroup f:expressions[0].groups)if(!f.getVars().contains(var)){
            left=left.sub(f);
            right=right.sub(f);
        }
        left.clean();
        if(left.isPolynomial()){
            Polynomial p=left.groups.get(0).getPolynomial();
            if(p.terms.size()==1){
                Term t=p.terms.get(0);
                ArrayList<Var> delete=new ArrayList<>();
                for (Var v:t.vars)if(v.sym.equals(var))delete.add(v);
                for (Var v:delete)t.vars.remove(v);
                right=right.div(new Expression(new Polynomial(t)));
                p=p.div(t);
                double power=delete.get(0).exp;
                if(right.groups.size()==1&&power!=1){
                    for (Function f:right.groups.get(0).functions)f.exp*=1/power;
                    p.terms.get(0).vars.get(0).exp=1;
                }
                left=new Expression(p);
            }
            temp.expressions[0]=left;temp.expressions[1]=right;
            temp.clean();
        }
        return temp;
    }
}
