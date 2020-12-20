package com.aleovas.mathlib;

import java.util.ArrayList;
import java.util.Collections;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.aleovas.mathlib.MiscFunctions.*;

public class Polynomial implements Comparable<Polynomial>{
    ArrayList<Term> terms=new ArrayList<>();
    Pattern regTerm=Pattern.compile("[+-]?(([0-9.]*([A-Za-z*]+(\\^(-)?\\d+(\\.\\d+)?)?)+)|([0-9.]+))");
    public int id;
    static int polyID;

    public Polynomial(String s){
        while(s.contains(" "))s=s.replace(" ","");
        while(s.contains("-+"))s=s.replace("-+","-");
        while(s.contains("+-"))s=s.replace("+-","-");
        while(s.contains("++"))s=s.replace("++","+");
        while(s.contains("--"))s=s.replace("--","+");
        Matcher matcher=regTerm.matcher(s);
        while(matcher.find())terms.add(new Term(s.substring(matcher.start(),matcher.end())));
        clean();
        id=polyID++;
    }
    public Polynomial(Term t){
        terms.add(t.copy());
        id=polyID++;
    }
    public Polynomial(double d){
        terms.add(new Term(d));
        id=polyID++;
    }
    public Polynomial(ArrayList<Term> ts){
        for (Term t:ts)terms.add(t.copy());
        id=polyID++;
    }
    public Polynomial(){
       this(0);
    }

    public void clean() {
        ArrayList<Term> temp=new ArrayList<Term>();
        //combine terms with the same variables and add their coefficients
        for(Term t:terms){
            boolean notFound=true;
            for(Term x:temp){
                //equals function was modified to only match variables and not the coefficient
                if(t.id!=x.id&&t.equals(x)){
                    x.coef+=t.coef;
                    notFound=false;
                }
            }
            if(notFound)temp.add(t);
        }
        terms.clear();
        for(Term t:temp)terms.add(t);
        temp.clear();
        //deletes any terms with the coefficient of zero
        for(Term t:terms)if(t.coef==0)temp.add(t);
        for(Term t:temp)terms.remove(t);
        //adds zero if the polynomial is empty
        if(terms.size()==0)terms.add(new Term(0));
        Collections.sort(terms);
    }
    public Polynomial copy(){
        return new Polynomial(terms);
    }
    public String toString(){
        if(terms.size()==0)return "0";
        String temp="";
        for(Term t:terms)temp+=t.toString();
        while (temp.contains(" "))temp=temp.replace(" ","");
        if(temp.charAt(0)=='+')return temp.substring(1).trim();
        return temp.trim();
    }
    public double getDegree(){
        clean();
        return terms.get(0).getDegree();
    }
    public double getDegree(String s){
        if(s.equals("")){
            return getDegree();
        }else {
            double exp =-9000;
            for(Term t:terms)exp=Math.max(exp,t.getDegree(s));
            return exp;
        }
    }
    public ArrayList<String> getVars(){
        ArrayList<String> list=new ArrayList<String>();
        for(Term t:terms)for (Var v:t.vars)if(!list.contains(v.sym))list.add(v.sym);
        return list;
    }
    public int getVarCount(){
        return getVars().size();
    }
    public ArrayList<Double> getFactors(){
        ArrayList<Double> l=new ArrayList<>();
        ArrayList<Double> lClean=new ArrayList<>();
        if(terms.get(terms.size()-1).getDegree()==0){
            double d=Math.abs(terms.get(terms.size()-1).coef);
            //Adding the last coefficient, its square root, and their negatives
            l.add(d);l.add(Math.sqrt(d));l.add(-d);l.add(-Math.sqrt(d));
            //Adding the results of divisions of the factors of the first and last coefficient
            ArrayList<Double> temp1=MiscFunctions.getFactors(d);
            ArrayList<Double> temp2=MiscFunctions.getFactors(terms.get(0).coef);
            for(double x1:temp1)for(double x2:temp2)if(!l.contains(x1/x2))l.add(x1/x2);
        }else l.add(0d);
        for(double d:l)if(!lClean.contains(d))lClean.add(d);
        return lClean;
    }
    public boolean equals(Polynomial p){
        return toString().equals(p.toString());
    }
    public String computerReadableString(){
        if(terms.size()==0)return "0";
        String temp="";
        for(Term t:terms)temp+=t.computerReadableString();
        if(temp.charAt(0)=='+')return temp.substring(1).trim();
        return temp.trim();
    }
    public boolean isConstant(){
        return getDegree()==0&&terms.size()==1&&getVarCount()==0;
    }
    public double val(){
        try{
            return terms.get(0).coef;
        }catch (ArrayIndexOutOfBoundsException e){
            return 0;
        }
    }

    public Polynomial add(Polynomial p){
        Polynomial temp1=copy();
        Polynomial temp2=p.copy();
        temp1.terms.addAll(temp2.terms);
        temp1.clean();
        return temp1;
    }
    public Polynomial mul(Term t){
        Polynomial temp=copy();
        //The ArrayList is to avoid a ConcurrentModificationException
        ArrayList<Term> ts=new ArrayList<>();
        for(Term x:temp.terms)ts.add(x.mul(t));
        temp=new Polynomial(ts);
        temp.clean();
        return temp;
    }
    public Polynomial mul(Polynomial p){
        Polynomial temp=new Polynomial(0);
        for(Term t:p.terms)temp=temp.add(this.mul(t));
        temp.clean();
        return temp;
    }
    public Polynomial mul(double d){
        if(d==1)return copy();
        if(d==0)return new Polynomial();
        return this.mul(new Term(d));
    }
    public Polynomial sub(Polynomial p){
        return this.add(p.mul(-1));
    }
    public Polynomial div(Term t){
        return this.mul(t.inverse());
    }
    public Polynomial[] div(Polynomial p){
        Polynomial pList[]=new Polynomial[2];
        Polynomial dividend=copy();
        Polynomial divisor=p.copy();
        Polynomial result=new Polynomial(0);
        Polynomial remainder;
        String s=""; //Symbol used to to get the degree, used to facilitate division by constant variables.
        if(divisor.getVarCount()==1)s=divisor.getVars().get(0);
        if(dividend.getDegree(s)<divisor.getDegree(s))remainder=divisor; //if division is impossible
        else if(divisor.getDegree(s)==0){
            //If dividing by a constant
            result=dividend.div(divisor.terms.get(0));
            remainder=new Polynomial(0);
        }else{
            //This is hard to explain. Look up a division algorithm on Wikipedia and understand it yourself
            while(dividend.getDegree(s)>=divisor.getDegree(s)){
//                Term temp=dividend.terms.get(0).div(divisor.terms.get(0));
//                result.terms.add(temp);
//                dividend=dividend.sub(divisor.mul(temp));
                Polynomial temp=new Polynomial(0);
                for(Term t:dividend.terms)if(t.getDegree(s)==dividend.getDegree(s))temp.terms.add(t);
                temp.clean();
                temp=temp.div(divisor.terms.get(0));
                result=result.add(temp);
                dividend=dividend.sub(divisor.mul(temp));
            }
            result.clean();
            remainder=dividend;
        }
        pList[0]=result;
        pList[1]=remainder;
        return pList;
    }
    public ArrayList<Polynomial> fac(){
        Polynomial temp=copy();
        temp.clean();
        ArrayList<Polynomial> list=new ArrayList<>();
        switch (getVarCount()){
            case 0:
                double d=temp.terms.get(0).coef;
                if(((int)d)==d){
                    //Prime factors are inserted as variables in a term because of its convenient
                    //clean function, which coalesces them and gives them an exponent. I know that
                    //this is a workaround
                    Term term=new Term(1);
                    ArrayList<Integer> factors=getPrimeFactors((int)d);
                    for(int i:factors)term.vars.add(new Var(1,i+""));
                    term.clean();
                    list.add(new Polynomial(term));
                }else throw new FloatFactoringException();
                break;
            case 1:
                String sym=getVars().get(0);
                while (true){
                    Polynomial oldTemp=temp;
                    ArrayList<Double> potentialFactors=temp.getFactors();
                    for(double x:potentialFactors) if(temp.subtitute(sym,x).toString().equals("0")){
                        //dividing by (x - factor)
                        Polynomial result=new Polynomial(sym+"-"+x);
                        temp=temp.div(result)[0];
                        list.add(result);
                        break; //There was a break here, but I don't know if it is needed
                    }
                    if(oldTemp.toString().equals(temp.toString())||temp.getDegree()<1){
                        list.add(temp);
                        break;
                    }
                }
                break;
            default:throw new MultivariateFactoringException();
        }
        Collections.sort(list);
        Collections.reverse(list);
        return list;
    }
    public Polynomial subtitute(String sym,double val){
        Polynomial p=copy();
        for(int i=0;i<p.terms.size();i++)p.terms.set(i,p.terms.get(i).substitute(sym,val));
        p.clean();
        return p;
    }
    public Polynomial subtitute(String sym,Term term){
        Polynomial p=copy();
        for(int i=0;i<p.terms.size();i++)p.terms.set(i,p.terms.get(i).substitute(sym,term));
        p.clean();
        return p;
    }
    public Polynomial subtitute(String sym,Polynomial p){
        Polynomial temp=new Polynomial(0);
        //for(Term t1:terms)for(Term t2:p.terms)temp=temp.add(new Polynomial(t1.substitute(sym,t2)));
        for(Term t:terms)temp=temp.add(t.subtitute(sym,p));
        temp.clean();
        return temp;
    }
    public Polynomial pow(int exp){
        //I can only support integer exponents at the moment
        Polynomial temp=new Polynomial(1);
        for(int i=0;i<exp;i++)temp=temp.mul(this);
        return temp;
    }

    @Override
    public int compareTo(Polynomial polynomial) {
        if(polynomial==null)return 1;else return -Double.compare(this.getDegree(),polynomial.getDegree());
    }
}
