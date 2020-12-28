package com.aleovas.mathlib;

import java.util.ArrayList;
import java.util.Collections;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.aleovas.mathlib.Function.Functions.*;
import static com.aleovas.mathlib.MiscFunctions.*;

//A class describing an abstraction of multiple "functions" multiplied together.
//Methods here are analogous to the ones in the Term class so refer to the documentation there.
public class FunctionGroup implements Comparable<FunctionGroup>{
    public ArrayList<Function> functions = new ArrayList<>();
    public double coef;
    public int id;
    static int fgID;
    //Currently provides preliminary support for single level trigonometric functions and logarithims
    //Future updates might provide further functions and the possibility for nesting functions.
    Pattern regFunction = Pattern.compile("((\\/?(sin|cos|tan|log|int|abs|sec|ctn|csc)(\\^[0-9\\.\\+\\-]+)?\\([*a-zA-Z0-9\\^\\-\\+\\.]*\\)))|(\\/?\\([*a-zA-Z0-9\\^\\-\\+\\.]*\\)(\\^[0-9\\.\\+\\-]+)?)|\\/[*a-zA-Z0-9\\.]+(\\^[0-9\\.\\+\\-]+)?");
    Pattern regCoef = Pattern.compile("\\^?([0-9\\.\\-\\+]+)(?=(sin|cos|tan|log|int|abs|sec|ctn|csc|\\/|\\())");

    public FunctionGroup(String s) {
        String temp = s;
        Matcher matcherF = regFunction.matcher(s);
        coef = 1;
        while (matcherF.find()) {
            String t = s.substring(matcherF.start(), matcherF.end());
            functions.add(new Function(t));
            temp = temp.replace(t, "");
        }
        Matcher matcherC = regCoef.matcher(s);
        while (matcherC.find()) {
            String t = s.substring(matcherC.start(), matcherC.end());
            if (t.contains("^")) {
                continue;
            } else {
                try {
                    coef = Double.parseDouble(t);
                } catch (Exception e) {
                }
                temp = temp.replace(t, "");
                break;
            }
        }
        if (temp.length() != 0) functions.add(new Function(new Polynomial(temp)));
        id = fgID++;
        clean();
    }
    public FunctionGroup(double coef, ArrayList<Function> functions) {
        this.coef = coef;
        this.functions = functions;
        id = fgID++;
    }
    public FunctionGroup(Polynomial p){
        coef=1;
        functions.add(new Function(p));
        id=fgID++;
    }

    void clean() {
        ArrayList<Function> temp = new ArrayList<>();
        if(coef==0)functions.clear();
        else {
            for (Function f : functions) f.clean();
            if (functions.size() > 1) {
                //merges duplicate functions and adds their exponents
                for (Function f : functions) {
                    boolean notFound = true;
                    for (Function x : temp) {
                        if (f.equalsWOExp(x) && f.id != x.id) {
                            x.exp += f.exp;
                            if(x.type==pol&&x.exp!=1)x.type=pow;
                            notFound = false;
                        }
                    }
                    if (notFound) temp.add(f);
                }
                functions.clear();
                functions.addAll(temp);
                temp.clear();

                //combines polynomials
                for (Function f : functions) f.powToPol();
                ArrayList<Polynomial> ps = new ArrayList<>();
                for (Function f : functions)
                    if (f.type == pol) {
                        ps.add(f.polynomial);
                        temp.add(f);
                    }
                if (ps.size() > 1) {
                    Polynomial t = onePolynomial;
                    for (Polynomial p : ps) t = t.mul(p);
                    functions.add(new Function(t));
                    for (Function f : temp) functions.remove(f);
                }
                temp.clear();
            }

            //gets rid of constant polynomials
            for (Function f : functions)
                if (f.type == Function.Functions.pol && f.polynomial.isConstant()) {
                    temp.add(f);
                    coef *= f.polynomial.val();
                }
            for (Function f : temp) functions.remove(f);
            temp.clear();

            if (functions.size() == 1 && functions.get(0).type == pol) {
                Polynomial t = functions.get(0).polynomial.mul(coef);
                functions.set(0, new Function(t));
                coef = 1;
            }

            for (Function f : functions) if (f.exp == 0) temp.add(f);
            for (Function f : temp) functions.remove(f);
            if(functions.size()==0){
                functions.add(new Function(new Polynomial(coef)));
                coef=1;
            }
            Collections.sort(functions);
        }
    }
    protected FunctionGroup copy() {
        ArrayList<Function> temp = new ArrayList<>();
        for (Function f : functions) temp.add(f.copy());
        return new FunctionGroup(coef, temp);
    }
    public String toString(){
        String temp;
        if(coef==1)temp="+";
        else temp=coef<0? doubleToHumanReadableString(coef):"+"+ doubleToHumanReadableString(coef);
        for(Function f:functions){
            if(f.type==pol&&(coef!=1||!(f.polynomial.terms.size()==1||functions.size()==1)))temp+="("+f+")";
            else temp+=f;
        }
        return temp;
    }
    public String computerReadableString(){
        String temp=coef!=1? doubleToComputerReadableString(coef):"";
        for(Function f:functions){
            if(f.type==pol&&(coef!=1||!(f.polynomial.terms.size()==1||functions.size()==1)))temp+="("+f.computerReadableString()+")";
            else temp+=f.computerReadableString();
        }
        return temp;
    }
    public boolean matchFunctions(FunctionGroup fg){
        if(functions.size()!=fg.functions.size())return false;
        FunctionGroup temp1=copy();
        FunctionGroup temp2=fg.copy();
        temp1.clean();temp2.clean();
        temp1.coef=1;temp2.coef=1;
        return temp1.computerReadableString().equals(temp2.computerReadableString());
    }
    public double getDegree(){
        double d=0;
        for(Function f:functions)d+=f.exp;
        return d;
    }
    public ArrayList<String> getVars(){
        ArrayList<String> temp=new ArrayList<>();
        for(Function f:functions)for(String s:f.getVars())if(!temp.contains(s))temp.add(s);
        return temp;
    }
    public boolean isPolynomial(){
        return functions.size()==1&&functions.get(0).type==pol;
    }
    public boolean isConstant(){
        return isPolynomial()&&functions.get(0).polynomial.isConstant();
    }
    public Polynomial getPolynomial(){
        return functions.get(0).polynomial.mul(coef);
    }
    public int getVarCount(){
        return getVars().size();
    }

    public FunctionGroup mul(FunctionGroup t) {
        FunctionGroup temp1 = this.copy();
        FunctionGroup temp2 = t.copy();
        temp1.coef *= temp2.coef;
        temp1.functions.addAll(temp2.functions);
        temp1.clean();
        return temp1;
    }
    public FunctionGroup mul(Polynomial p){
     FunctionGroup temp=this.copy();
     temp.functions.add(new Function(p));
     temp.clean();
     return temp;
    }
    public FunctionGroup mul(double d) {
        FunctionGroup temp = this.copy();
        temp.coef *= d;
        return temp;
    }
    public FunctionGroup inverse() {
        if (coef == 0) throw new MiscFunctions.DivideByZeroException("You don't divide by zero");
        FunctionGroup temp = this.copy();
        temp.coef = 1 / temp.coef;
        for (Function v : temp.functions) v.exp *= -1;
        return temp;
    }
    public FunctionGroup div(FunctionGroup t) {
        return this.mul(t.inverse());
    }
    public Expression div(Polynomial p){
        if(isPolynomial()){
            ArrayList<FunctionGroup> temp1=new ArrayList<>();
            Polynomial[] temp2=getPolynomial().div(p);
            temp1.add(new FunctionGroup(temp2[0]));
            FunctionGroup temp3=new FunctionGroup(temp2[1]);
            temp3.functions.add(new Function(p,-1));
            temp1.add(temp3);
            return new Expression(temp1);
        }
        return new Expression(this.div(new FunctionGroup(p)));
    }
    public FunctionGroup substitute(String s, double d) {
        ArrayList<Function> temp=new ArrayList<>();
        for(Function f: functions)temp.add(f.substitute(s,d));
        FunctionGroup t=new FunctionGroup(coef,temp);
        t.clean();
        return t;
    }
    public FunctionGroup subtitute(String s, Polynomial p) {
        ArrayList<Function> temp=new ArrayList<>();
        for(Function f: functions)temp.add(f.substitute(s,p));
        FunctionGroup t=new FunctionGroup(coef,temp);
        t.clean();
        return t;
    }
    public FunctionGroup pow(double d) {
        FunctionGroup temp = this.copy();
        temp.coef = Math.pow(temp.coef, d);
        for (Function v : temp.functions) v.exp *= d;
        temp.clean();
        return temp;
    }
    public FunctionGroup fac(){
        if (isPolynomial()){
            ArrayList<Function> temp=new ArrayList<>();
            for(Polynomial p:getPolynomial().fac())temp.add(new Function(p));
            return new FunctionGroup(1,temp);
        }
        return this;
    }

    @Override
    public int compareTo(FunctionGroup functionGroup) {
        if(functionGroup==null)return 1;else return -Double.compare(this.getDegree(), functionGroup.getDegree());
    }
}
