package com.aleovas.mathlib;

import java.util.ArrayList;
import java.util.Collections;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.aleovas.mathlib.MiscFunctions.*;

public class Polynomial implements Comparable<Polynomial>{
    ArrayList<Term> terms=new ArrayList<>(); //The polynomial class is basically an ArrayList of terms.
    //This regex pattern matches terms in a polynomial and passes the string of each term into the
    //constructor of the Term class to make a new Term object. Thus the process of constructing
    //a polynomial can be thought of as a series of abstractions from Polynomial → Term → Variable.
    private Pattern regTerm=Pattern.compile("[+-]?(([0-9.]*([A-Za-z*]+(\\^(-)?\\d+(\\.\\d+)?)?)+)|([0-9.]+))");
    public int id; // Polynomial ID is currently only used for debug purposes. Might have some further use in the future
    private static int polyID; //This is incremented every time a new polynomial is created and is used to create polynomial IDs

    public Polynomial(String s){
        //Uses regex to find each term in the string S and passes the term's string to the constructor of the Term class
        //It first replaces empty spaces and deals with consecutive additions and subtractions.
        while(s.contains(" "))s=s.replace(" ","");
        while(s.contains("-+"))s=s.replace("-+","-"); //every adjacent - and + should combine together to become a -
        while(s.contains("+-"))s=s.replace("+-","-");
        while(s.contains("++"))s=s.replace("++","+"); //multiple + would probably be ignored, but are removed for consistency
        while(s.contains("--"))s=s.replace("--","+"); //two wrongs do make a right... In mathematics at least
        Matcher matcher=regTerm.matcher(s);
        while(matcher.find())terms.add(new Term(s.substring(matcher.start(),matcher.end())));
        clean();
        id=polyID++;
    }
    public Polynomial(Term t){
        //Creates a polynomial containing only the term T.
        terms.add(t.copy());
        id=polyID++;
    }
    public Polynomial(double d){
        //Creates a polynomial containing a constant number
        terms.add(new Term(d));
        id=polyID++;
    }
    public Polynomial(ArrayList<Term> ts){
        //Creates a polynomial from an ArrayList of terms
        for (Term t:ts)terms.add(t.copy());
        id=polyID++;
    }
    public Polynomial(){
        //The default constructor should form a zero polynomial.
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
        //Sorts terms by their degree (the sum of the exponents of all their variables)
        Collections.sort(terms);
    }
    public Polynomial copy(){
        //Returns a copy of this polynomial
        return new Polynomial(terms);
    }
    public String toString(){
        //Returns a human-readable string representation of the polynomial
        if(terms.size()==0)return "0";
        String temp="";
        for(Term t:terms)temp+=t.toString();
        while (temp.contains(" "))temp=temp.replace(" ","");
        if(temp.charAt(0)=='+')return temp.substring(1).trim();
        return temp.trim();
    }
    public double getDegree(){
        //The degree of a polynomial is the degree of its highest degree term. and since the
        //clean function sorts the terms it is always the first term
        clean();
        return terms.get(0).getDegree();
    }
    public double getDegree(String s){
        if(s.equals("")){
            return getDegree();
        }else {
            double exp = 0;
            for(Term t:terms)exp=Math.max(exp,t.getDegree(s));
            for(Term t:terms){
                double deg = t.getDegree(s);
                if (deg<0&&exp<=0)exp=Math.min(exp,t.getDegree(s));
            }
            return exp;
        }
    }
    public ArrayList<String> getVars(){
        //Gets the list of all variables found in the polynomial
        ArrayList<String> list=new ArrayList<String>();
        for(Term t:terms)for (Var v:t.vars)if(!list.contains(v.sym))list.add(v.sym);
        return list;
    }
    public int getVarCount(){
        return getVars().size();
    }
    public ArrayList<Double> getFactors(){
        //Gets a list of possible factors for the polynomial.
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
            if(getDegree()==2){
                //Special case for the quadratic formula
                double a=0, b=0, c=0;
                boolean brk=false;
                for(Term t: terms){
                    if(!isInteger(t.getDegree())){
                        //If one of the terms has a fractional degree or a degree below zero then break;
                        brk=true;
                        break;
                    }
                    switch ((int) t.getDegree()){
                        case 0: c=t.coef;break;
                        case 1: b=t.coef;break;
                        case 2: a=t.coef;break;
                    }
                }
                double det = Math.sqrt(b * b - 4 * a * c);
                if(det>=0&&!brk){
                    double x1=(-b+det)/2*a;
                    double x2=(-b-det)/2*a;
                }
            }
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

    //Functions for Polynomial manipulation. All these return a new object without modifying the original.
    public Polynomial add(Polynomial p){
        //Adds a polynomial P to this polynomial. It makes copies of both  polynomials, adds all the
        //terms from the second one to the first one, and consolidates terms.
        Polynomial temp1=copy();
        Polynomial temp2=p.copy();
        temp1.terms.addAll(temp2.terms);
        temp1.clean();
        return temp1;
    }
    public Polynomial mul(Term t){
        //Multiplies every term of the polynomial by the term T
        Polynomial temp=copy();
        ArrayList<Term> ts=new ArrayList<>(); //Uses an ArrayList is to avoid a ConcurrentModificationException
        for(Term x:temp.terms)ts.add(x.mul(t));
        temp=new Polynomial(ts);
        temp.clean();
        return temp;
    }
    public Polynomial mul(Polynomial p){
        //Multiplying a polynomial with another polynomial is the same as the sum of the result of
        //multiplying the polynomial with every term of the second polynomial
        Polynomial temp=new Polynomial(0);
        for(Term t:p.terms)temp=temp.add(this.mul(t));
        temp.clean();
        return temp;
    }
    public Polynomial mul(double d){
        //This one reuses the code for multiplying by a term, with special cases for 0 and 1 that
        //should be faster. It might be more optimized make a new method in the future.
        if(d==1)return copy();
        if(d==0)return new Polynomial();
        return this.mul(new Term(d));
    }
    public Polynomial sub(Polynomial p){
        //Subtracting a polynomial is the same as adding the polynomial after multiplying by -1
        return this.add(p.mul(-1));
    }
    public Polynomial div(Term t){
        //Dividing by a term is just the same as multiplying by the inverse of that term
        return this.mul(t.inverse());
    }
    public Polynomial[] div(Polynomial p){
        //Due to the nature of polynomial division, there is a chance that there will be a remainder.
        //With that in mind, this method returns an array containing the result and the remainder.
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
            //This is hard to explain. The following wikipedia link should explain the algorithm better than I can.
            //https://en.wikipedia.org/wiki/Polynomial_long_division
            while(dividend.getDegree(s)>=divisor.getDegree(s)){
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
        //Returns an arraylist containing all the factors of the polynomial. If the polynomial consists only of a constant, it returns it's prime factors
        //TODO consider splitting off factoring constant numbers into a separate function, it seems only tangentially related and might be an unexpected behavior
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
            default:throw new MultivariateFactoringException(); //TODO implement factoring multivariate polynomials
        }
        Collections.sort(list);
        Collections.reverse(list);
        return list;
    }
    public Polynomial subtitute(String sym,double val){
        //Substitutes a numeric value for one of the variables.
        Polynomial p=copy();
        for(int i=0;i<p.terms.size();i++)p.terms.set(i,p.terms.get(i).substitute(sym,val));
        p.clean();
        return p;
    }
    public Polynomial subtitute(String sym,Term term){
        //Substitutes a term value for one of the variables.
        Polynomial p=copy();
        for(int i=0;i<p.terms.size();i++)p.terms.set(i,p.terms.get(i).substitute(sym,term));
        p.clean();
        return p;
    }
    public Polynomial subtitute(String sym,Polynomial p){
        //Substitutes a polynomial value for one of the variables.
        Polynomial temp=new Polynomial(0);
        //for(Term t1:terms)for(Term t2:p.terms)temp=temp.add(new Polynomial(t1.substitute(sym,t2)));
        for(Term t:terms)temp=temp.add(t.subtitute(sym,p));
        temp.clean();
        return temp;
    }
    public Polynomial pow(int exp){
        //Raises the polynomial to a certain exponent.
        //I can only support integer exponents at the moment
        //TODO look into the feasibility of implementing fractional exponents.
        Polynomial temp=new Polynomial(1);
        for(int i=0;i<exp;i++)temp=temp.mul(this);
        return temp;
    }

    @Override
    public int compareTo(Polynomial polynomial) {
        //Sorts polynomials by the degree of each polynomial.
        if(polynomial==null)return 1;else return -Double.compare(this.getDegree(),polynomial.getDegree());
    }
}
