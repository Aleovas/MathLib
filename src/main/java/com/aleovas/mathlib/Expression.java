package com.aleovas.mathlib;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.aleovas.mathlib.MiscFunctions.*;


public class Expression {
    ArrayList<FunctionGroup> groups=new ArrayList<>();
    public int id;
    static int expID;
    //Pattern regExp=Pattern.compile("((\\/?(\\-|\\+)?(\\/?(((sin|cos|tan|log|int|abs|sec|ctn|csc)(\\^(\\+|\\-)?\\d(\\.\\d)?)?)(\\([a-zA-Z0-9\\-\\+\\>\\^\\.]*\\))+|(\\([a-zA-Z0-9\\-\\+\\>\\^\\.]*\\))(\\^(\\+|\\-)?\\d(\\.\\d)?)?)+)+))|([a-zA-Z0-9\\-\\+\\>\\^\\.]+(?!\\)))((\\/?(\\-|\\+)?(\\/?(((sin|cos|tan|log|int|abs|sec|ctn|csc)(\\^(\\+|\\-)?\\d(\\.\\d)?)?)(\\([a-zA-Z0-9\\-\\+\\>\\^\\.]*\\))+|(\\([a-zA-Z0-9\\-\\+\\>\\^\\.]*\\))(\\^(\\+|\\-)?\\d(\\.\\d)?)?)+)+))?");
    Pattern regExp=Pattern.compile("^((?!((\\+|\\-)?[a-z0-9\\.]*)?(sin|cos|tan|log|int|abs|sec|ctn|csc|\\()).)+|(((\\/?(\\-|\\+)?(\\/?(((sin|cos|tan|log|int|abs|sec|ctn|csc)(\\^(\\+|\\-)?\\d(\\.\\d)?)?)(\\([a-zA-Z0-9\\-\\+\\>\\^\\.]*\\))([a-z]*)+|(\\([a-zA-Z0-9\\-\\+\\>\\^\\.]*\\))(\\^(\\+|\\-)?\\d(\\.\\d)?)?)+)+))|([a-zA-Z0-9\\-\\+\\>\\^\\.]+(?!\\)))((\\/?(\\-|\\+)?(\\/?(((sin|cos|tan|log|int|abs|sec|ctn|csc)(\\^(\\+|\\-)?\\d(\\.\\d)?)?)(\\([a-zA-Z0-9\\-\\+\\>\\^\\.]*\\))+|(\\([a-zA-Z0-9\\-\\+\\>\\^\\.]*\\))(\\^(\\+|\\-)?\\d(\\.\\d)?)?)+)+))?)");
    public Expression(String s){
        while(s.contains(" "))s=s.replace(" ","");
        while(s.contains("-+"))s=s.replace("-+","-");
        while(s.contains("+-"))s=s.replace("+-","-");
        while(s.contains("++"))s=s.replace("++","+");
        while(s.contains("--"))s=s.replace("--","+");
        Matcher matcher=regExp.matcher(s);
        while(matcher.find()){
            String t=s.substring(matcher.start(),matcher.end());
            groups.add(new FunctionGroup(t));
            s.replace(t,"");
        }
        clean();
        id=expID++;
    }
    public Expression(FunctionGroup fg){
        groups.add(fg.copy());
        id=expID++;
    }
    public Expression(ArrayList<FunctionGroup> fgs){
        for(FunctionGroup fg:fgs)groups.add(fg.copy());
        id=expID++;
    }
    public Expression(Polynomial p){
        groups.add(new FunctionGroup(p));
        id=expID++;
    }
    public Expression(double d){
        this(new Polynomial(d));
    }
    public Expression(){
        this(new Polynomial());
    }

    void clean() {
        ArrayList<FunctionGroup> temp=new ArrayList<>();
        if(groups.size()>1){
            Polynomial t=zeroPolynomial;
            ArrayList<FunctionGroup> remove=new ArrayList<>();
            for(FunctionGroup f:groups)if(f.isPolynomial()){
                t=t.add(f.getPolynomial());
                remove.add(f);
            }
            for(FunctionGroup f:remove)groups.remove(f);
            groups.add(new FunctionGroup(t));
        }
        //combine FG with the same variables and add their coefficients

        for(FunctionGroup t:groups){
            boolean notFound=true;
            for(FunctionGroup x:temp){
                //equals function was modified to only match functions and not the coefficient
                if(t.id!=x.id&&t.matchFunctions(x)){
                    x.coef+=t.coef;
                    notFound=false;
                }
            }
            if(notFound)temp.add(t);
        }
        groups.clear();
        for(FunctionGroup t:temp)groups.add(t);
        temp.clear();
        for(FunctionGroup fg:groups)fg.clean();
        //deletes any groups with the coefficient of zero
        for(FunctionGroup t:groups)if(t.coef==0)temp.add(t);
        for(FunctionGroup t:temp)groups.remove(t);
        //adds zero if the expression is empty
        if(groups.size()==0)groups.add(new FunctionGroup(new Polynomial(0)));
        Collections.sort(groups);
    }
    public Expression copy(){
        return new Expression(groups);
    }
    public String toString(){
        if(groups.size()==0)return "0";
        String temp="";
        for(FunctionGroup t:groups)temp+=t.toString();
        if(temp.charAt(0)=='+')return temp.substring(1).trim();
        return temp.trim();
    }
    public ArrayList<String> getVars(){
        ArrayList<String> temp=new ArrayList<>();
        for(FunctionGroup f:groups)for(String s:f.getVars())if(!temp.contains(s))temp.add(s);
        return temp;
    }
    public double getDegree(){
        clean();
        return groups.get(0).getDegree();
    }
    public int getVarCount(){
        return getVars().size();
    }
    public boolean equals(Polynomial p){
        return toString().equals(p.toString());
    }
    public String computerReadableString(){
        if(groups.size()==0)return "0";
        String temp="";
        for(FunctionGroup t:groups)temp+=t.computerReadableString();
        if(temp.charAt(0)=='+')return temp.substring(1).trim();
        return temp.trim();
    }
    public boolean isPolynomial(){
        return groups.size()==1&&groups.get(0).isPolynomial();
    }
    public boolean isPolynomials(){
        for(FunctionGroup f:groups)if(!f.isPolynomial())return false;
        return true;
    }


    public Expression add(Expression e){
        Expression temp=copy();
        temp.groups.addAll(e.copy().groups);
        temp.clean();
        return temp;
    }
    public Expression add(FunctionGroup f){
        return this.add(new Expression(f));
    }
    public Expression mul(FunctionGroup fg){
        ArrayList<FunctionGroup> fgs=new ArrayList<>();
        for(FunctionGroup x:groups)fgs.add(x.mul(fg));
        Expression temp=new Expression(fgs);
        temp.clean();
        return temp;
    }
    public Expression mul(Expression e){
        Expression temp=new Expression();
        for(FunctionGroup fg:e.groups){
            Expression t=this.mul(fg);
            temp=temp.add(t);
        }
        temp.clean();
        return temp;
    }
    public Expression mul(double d){
        Expression temp=copy();
        for (FunctionGroup fg:temp.groups)fg.coef*=d;
        return temp;
    }
    public Expression sub(Expression e){
        return this.add(e.mul(-1));
    }
    public Expression sub(FunctionGroup f){
        return this.sub(new Expression(f));
    }
    public Expression fac(){
        Expression temp1=copy();
        ArrayList<FunctionGroup> temp2=new ArrayList<>();
        for(FunctionGroup f:temp1.groups)temp2.add(f.fac());
        return new Expression(temp2);
    }
    public Expression div(FunctionGroup f){
        return this.mul(f.inverse());
    }
    public Expression div(Expression e){
        if(e.groups.size()==1){
            if(e.isPolynomial()){
                Expression temp=new Expression();
                for(FunctionGroup f:groups)temp=temp.add(f.div(e.groups.get(0).getPolynomial()));
                temp.clean();
                return temp;
            }
            else return div(e.groups.get(0));
        }
        throw new ExpressionDivisionException();
    }
    public Expression subtitute(String sym, double d){
        Expression e=copy();
        for(int i=0;i<e.groups.size();i++)e.groups.set(i,e.groups.get(i).substitute(sym,d));
        e.clean();
        return e;
    }
    public Expression subtitute(String sym, Polynomial p){
        Expression temp=new Expression();
        for(FunctionGroup f:groups)temp=temp.add(new Expression(f.subtitute(sym, p)));
        temp.clean();
        return temp;
    }
    public Expression pow(int exp){
        if(exp==0)return new Expression(1);
        Expression temp=this;
        for(int i=1;i<exp;i++)temp=temp.mul(this);
        return temp;
    }

}
