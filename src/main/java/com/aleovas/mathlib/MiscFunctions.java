package com.aleovas.mathlib;

import java.util.ArrayList;
import static java.lang.Math.sqrt;

public class MiscFunctions {
    //The first variable denote some settings which may be user-modifiable in the future.
    public static String mulSymbol="·";
    public static boolean singleCharVariable=true;
    public static boolean replaceDecimalsWithFractions=true;
    public static boolean replaceDecimalsWithRoots=true;
    public static int defaultRound=6; //Default number of positions to round to
                                      //TODO enable this to be user-modifiable

    //Various utility functions
    public static boolean isNumeric(String s){
        try{
            Double.parseDouble(s);
        }catch(Exception e){
            return false;
        }
        return true;
    }
    public static boolean isInteger(double d){
        return ((int) d)==d;
    }
    public static String getExponent(Double exp){
        //Returns a human-readable string that is supposed to represent the exponent
        //TODO fix this function so that it correctly handles square root exponents
        if (exp==1) return "";
        String temp= doubleToHumanReadableString(exp);
        temp = temp.replace("1", "¹");
        temp = temp.replace("2", "²");
        temp = temp.replace("3", "³");
        temp = temp.replace("4", "⁴");
        temp = temp.replace("5", "⁵");
        temp = temp.replace("6", "⁶");
        temp = temp.replace("7", "⁷");
        temp = temp.replace("8", "⁸");
        temp = temp.replace("9", "⁹");
        temp = temp.replace("0", "⁰");
        temp = temp.replace("-", "⁻");
        temp = temp.replace(".", "⋅");
        return temp;
    }
    public static double getNumber(String str){
        //Handles certain cases where the coefficient is implicitly understood
        switch(str){
            case "+":return 1;
            case "":return 1;
            case "-":return -1;
            default:return Double.parseDouble(str);
        }
    }
    public static String doubleToHumanReadableString(double d){
        //Converts doubles to a more human-readable string by comparing double to common values
        //which have a unicode character.
        if (replaceDecimalsWithFractions){
            if(round(d,defaultRound)==round(1f/3,defaultRound))return "⅓";// 1/3
            if(d==0.5)return "½";
            if(d==0.25) return "¼";
            if(d==0.75) return "¾";
            if(round(d,defaultRound)==round(1f/7,defaultRound)) return "⅐";
            if(round(d,defaultRound)==round(1f/9,defaultRound)) return "⅑";
            if(round(d,defaultRound)==round(1f/6,defaultRound)) return "⅙";
            if(round(d,defaultRound)==round(5f/6,defaultRound)) return "⅚";
            if(round(d,defaultRound)==round(1f/8,defaultRound)) return "⅚";
            if(round(d,defaultRound)==round(3f/8,defaultRound)) return "⅜";
            if(round(d,defaultRound)==round(5f/8,defaultRound)) return "⅝";
            if(round(d,defaultRound)==round(7f/8,defaultRound)) return "⅞";
            if(round(d,defaultRound)==round(-1f/3,defaultRound))return "-⅓";
            if(d==-0.5)return "-½";
            if(d==-0.25) return "-¼";
            if(d==-0.75) return "-¾";
            if(round(d,defaultRound)==round(-1f/7,defaultRound)) return "-⅐";
            if(round(d,defaultRound)==round(-1f/9,defaultRound)) return "-⅑";
            if(round(d,defaultRound)==round(-1f/6,defaultRound)) return "-⅙";
            if(round(d,defaultRound)==round(-5f/6,defaultRound)) return "-⅚";
            if(round(d,defaultRound)==round(-1f/8,defaultRound)) return "-⅚";
            if(round(d,defaultRound)==round(-3f/8,defaultRound)) return "-⅜";
            if(round(d,defaultRound)==round(-5f/8,defaultRound)) return "-⅝";
            if(round(d,defaultRound)==round(-7f/8,defaultRound)) return "-⅞";
        }
        if (replaceDecimalsWithRoots){
            if(round(d,defaultRound)==round(sqrt(2),defaultRound))return "√2";
            if(round(d,defaultRound)==round(sqrt(3),defaultRound))return "√3";
            if(round(d,defaultRound)==round(sqrt(5),defaultRound))return "√5";
            if(round(d,defaultRound)==round(sqrt(7),defaultRound))return "√7";
            if(round(d,defaultRound)==round(sqrt(10),defaultRound))return "√10";
            if(round(d,defaultRound)==round(sqrt(8),defaultRound))return "√8";
            if(round(d,defaultRound)==round(sqrt(11),defaultRound))return "√11";
            if(round(d,defaultRound)==round(1f/sqrt(2),defaultRound))return "1/√2";
            if(round(d,defaultRound)==round(1f/sqrt(3),defaultRound))return "1/√3";
            if(round(d,defaultRound)==round(1f/sqrt(5),defaultRound))return "1/√5";
            if(round(d,defaultRound)==round(1f/sqrt(7),defaultRound))return "1/√7";
            if(round(d,defaultRound)==round(1f/sqrt(8),defaultRound))return "1/√8";
            if(round(d,defaultRound)==round(-sqrt(2),defaultRound))return "-√2";
            if(round(d,defaultRound)==round(-sqrt(3),defaultRound))return "-√3";
            if(round(d,defaultRound)==round(-sqrt(5),defaultRound))return "-√5";
            if(round(d,defaultRound)==round(-sqrt(7),defaultRound))return "-√7";
            if(round(d,defaultRound)==round(-sqrt(10),defaultRound))return "-√10";
            if(round(d,defaultRound)==round(-sqrt(8),defaultRound))return "-√8";
            if(round(d,defaultRound)==round(-sqrt(11),defaultRound))return "-√11";
            if(round(d,defaultRound)==round(-1f/sqrt(2),defaultRound))return "-1/√2";
            if(round(d,defaultRound)==round(-1f/sqrt(3),defaultRound))return "-1/√3";
            if(round(d,defaultRound)==round(-1f/sqrt(5),defaultRound))return "-1/√5";
            if(round(d,defaultRound)==round(-1f/sqrt(7),defaultRound))return "-1/√7";
            if(round(d,defaultRound)==round(-1f/sqrt(8),defaultRound))return "-1/√8";
        }
        if(((int) d)==d)return ((int)d)+"";else return d+"";
    }
    public static String doubleToComputerReadableString(double d){
        //Returns a computer readable string; Basically just removes the decimal point if the double
        //is technically an integer
        if(((int) d)==d)return ((int)d)+"";else return d+"";
    }
    public static ArrayList<Double> getFactors(double d){
        //Finds whole number factors of a number
        ArrayList<Double> l=new ArrayList<Double>();
        double x=1;
        while(x<=d){
            if(d%x==0){
                l.add(x);
                l.add(-x);
            }
            x++;
        }
        return l;
    }
    public static ArrayList<Integer> getPrimeFactors(int d){
        ArrayList<Integer> list=new ArrayList<>();
        double original=d;
        out:while(d>1){
            int x=0;
            while(true){
                x++;
                if(d%x==0&&x!=1){
                    list.add(x);
                    d/=x;
                    x=1;
                }
                if(x>original||d==1)break out;
            }
        }
        return list;
    }
    public static double round(double d, int i){
        double temp=d*Math.pow(10,i);
        temp=Math.round(temp);
        temp=temp/Math.pow(10,i);
        return temp;
    }

    //Various useful exceptions
    public static class DivideByZeroException extends RuntimeException{
        public DivideByZeroException() {
        }

        public DivideByZeroException(String s) {
            super(s);
        }

        public DivideByZeroException(String s, Throwable throwable) {
            super(s, throwable);
        }

        public DivideByZeroException(Throwable throwable) {
            super(throwable);
        }

    }
    public static class FloatFactoringException extends RuntimeException{
        public FloatFactoringException() {
        }

        public FloatFactoringException(String s) {
            super(s);
        }

        public FloatFactoringException(String s, Throwable throwable) {
            super(s, throwable);
        }

        public FloatFactoringException(Throwable throwable) {
            super(throwable);
        }

    }
    public static class MultivariateFactoringException extends RuntimeException{
        //TODO implement it instead of returning an error
        public MultivariateFactoringException() {
        }

        public MultivariateFactoringException(String s) {
            super(s);
        }

        public MultivariateFactoringException(String s, Throwable throwable) {
            super(s, throwable);
        }

        public MultivariateFactoringException(Throwable throwable) {
            super(throwable);
        }

    }
    public static class ExpressionDivisionException extends RuntimeException{}
    public static class PolynomialFloatPowerException extends RuntimeException{}
    public static class InvalidEquationException extends RuntimeException{}
    public static class GeneralException extends RuntimeException{}

    //Common polynomials that are used in other functions
    public final static Polynomial onePolynomial=new Polynomial(1);
    public final static Polynomial zeroPolynomial=new Polynomial(0);
}
