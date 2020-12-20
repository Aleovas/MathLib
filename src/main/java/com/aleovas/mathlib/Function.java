package com.aleovas.mathlib;

import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.aleovas.mathlib.Function.Functions.*;
import static com.aleovas.mathlib.MiscFunctions.*;

public class Function implements Comparable<Function>{
    @Override
    public int compareTo(Function function) {
        if(function==null)return 1;else return -Double.compare(this.exp,function.exp);
    }

    public enum Functions {pol,sin,cos,tan,log,pow,exp,abs,Int,sec,ctn,csc}
    public Functions type;
    public Polynomial polynomial;
    public Polynomial expPolynomial;
    public double exp;
    public int id;
    public static int functionID;
    int clean=0;
    Pattern regPolynomial=Pattern.compile("(([A-Za-z0-9\\.\\+\\-\\^]+)(?=\\)))|(^(?!(sin|cos|log|tan|int|abs|sec|ctn|csc))([A-Za-z0-9\\.\\+\\-\\^]+)$)",Pattern.CASE_INSENSITIVE);
    Pattern regExp=Pattern.compile("(?<=(sin|cos|tan|log|int|abs|sec|ctn|csc|\\))\\^)(\\+|\\-)?(([0-9]+(\\.[0-9]+)?)|(\\.[0-9]+))",Pattern.CASE_INSENSITIVE);
    Pattern regType=Pattern.compile("(sin|cos|tan|log|int|abs|sec|ctn|csc)",Pattern.CASE_INSENSITIVE);
    public Function(Polynomial polynomial){
        this(polynomial, 1);
    }
    public Function(Polynomial polynomial, double exp){
        this.polynomial=polynomial;
        this.exp=exp;
        type=exp==1? pol: pow;
        id=functionID++;
        clean();
    }
    public Function(String s){
        Matcher matcherP=regPolynomial.matcher(s.replace("/",""));Matcher matcherE=regExp.matcher(s);Matcher matcherT=regType.matcher(s);
        if(matcherP.find())polynomial=new Polynomial(s.replace("/","").substring(matcherP.start(),matcherP.end()));
        if(matcherE.find())try {
            exp=Double.parseDouble(s.substring(matcherE.start(),matcherE.end()));
        }catch (Exception e){
            exp=1;
        }else exp=1;
        if(matcherT.find()){
            String temp=s.substring(matcherT.start(),matcherT.end());
            temp=temp.toLowerCase();
            if(temp.equals("int")) type=Functions.Int;else type=Functions.valueOf(temp);
        }else type= pow;
        if(s.contains("/"))exp*=-1;
        clean();
    }
    public Function(Polynomial polynomial, double exp, Functions t){
        this.polynomial=polynomial;
        this.exp=exp;
        this.type=Functions.valueOf(t.name());
        id=functionID++;
        clean();
    }

    public String toString(){
        String temp="";
        if(type== pol)return polynomial.toString();
        if(type== pow){
            temp="("+polynomial.toString()+")"+getExponent(exp);
            return temp;
        }
        if(type==Functions.exp){
            if(expPolynomial.terms.size()==1)temp+=expPolynomial.toString();
            else temp+=("("+expPolynomial+")");
            if(polynomial.terms.size()==1)temp+=("^"+polynomial);else temp+=("^("+polynomial+")");
            return temp;
        }
        temp=type.name()+getExponent(exp)+"("+polynomial+")";
        return temp;
    }
    public String computerReadableString(){
        String temp="";
        if(type== pol)return polynomial.computerReadableString();
        if(type== pow){
            temp="("+polynomial.computerReadableString()+")^"+ doubleToComputerReadableString(exp);
            return temp;
        }
        if(type==Functions.exp){
            if(expPolynomial.terms.size()==1)temp+=expPolynomial.computerReadableString();
            else temp+=("("+expPolynomial+")");
            if(polynomial.terms.size()==1)temp+=("^"+polynomial.computerReadableString());else temp+=("^("+polynomial.computerReadableString()+")");
            return temp;
        }
        temp=type.name()+"^"+ doubleToComputerReadableString(exp)+"("+polynomial.computerReadableString()+")";
        return temp;
    }
    public void clean(){
        if(clean<7){polynomial.clean();
        if(type==pow&&exp==1)type=pol;
        if(type==pol&&exp!=1)type=pow;
        if(type==pow&&polynomial.terms.size()==1){
            polynomial.terms.set(0,polynomial.terms.get(0).pow(exp));
            type=pol;
            exp=1;
        }
        if(polynomial==null)polynomial=zeroPolynomial;
        else if(exp==0){
            polynomial=onePolynomial;
            exp=1;
            type=pol;
        }
        if(type==abs&&exp%2==0)type=pow;
        switch (type){
            case sec:type=cos;exp*=-1;
                break;
            case ctn:type=tan;exp*=-1;
                break;
            case csc:type=sin;exp*=-1;
                break;
        }
        if(polynomial.isConstant()&&type!=pol){
            double d=0;
            switch (type){
                case sin:d = round(Math.sin(Math.toRadians(polynomial.val())),4);
                    break;
                case cos:d = round(Math.cos(Math.toRadians(polynomial.val())),4);
                    break;
                case tan:d =polynomial.val()%90==0?(polynomial.val()%180==0?0:Double.POSITIVE_INFINITY): round(Math.tan(Math.toRadians(polynomial.val())),4);
                    break;
                case log:d = Math.log10(polynomial.val());
                    break;
                case pow:d = polynomial.val();
                    break;
                case exp:d = polynomial.val();
                    break;
                case abs:d = Math.abs(polynomial.val());
                    break;
                case Int:d= Math.floor(polynomial.val());
                    break;
            }

            polynomial=new Polynomial(Math.pow(d,exp));
            if(type!=Functions.exp){
                exp = 1;
                type = pol;
            }else type= pow;
        }}
        clean++;
    }
    public Function copy(){
        return new Function(polynomial.copy(),exp,type);
    }
    public void powToPol(){
        if(type== pow&&isInteger(exp)&&exp>0){
            Polynomial temp=onePolynomial;
            boolean negative=false;
            for(int i=0;i<exp;i++) temp=temp.mul(polynomial);
            polynomial=temp;
            exp=1;
            type= pol;
        }
    }
    public void polToPow(){
        if(type== pol){
            ArrayList<Polynomial> temp=polynomial.fac();
            //todo implement this
        }
        clean();
    }
    public boolean equals(Function function){
        return exp==function.exp&&type==function.type&&polynomial.equals(function.polynomial);
    }
    public boolean equalsWOExp(Function function){
        return type==function.type&&polynomial.equals(function.polynomial);
    }
    public boolean match(Function function){
        return polynomial.equals(function.polynomial);
    }
    public Function substitute(String sym, double d){
        Function temp=copy();
        temp.polynomial=temp.polynomial.subtitute(sym, d);
        temp.clean();
        return temp;
    }
    public Function substitute(String sym,Polynomial p){
        Function temp=copy();
        temp.polynomial=temp.polynomial.subtitute(sym, p);
        temp.clean();
        return temp;
    }
    public ArrayList<String> getVars(){
        return polynomial.getVars();
    }
}
