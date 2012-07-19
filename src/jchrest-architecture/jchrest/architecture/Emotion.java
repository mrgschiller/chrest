// Copyright (c) 2011-2012, Marvin Schiller,
// with contributions by Peter C. R. Lane.
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.architecture;

import java.io.*;
import java.lang.Math;
import java.util.*;
import java.util.HashMap;

/**
 * Represents emotions as vectors of basic emotions
 *
 * @author Marvin Schiller
 */
 
public class Emotion {
    private HashMap<BasicEmotion,Double> _components = new HashMap<BasicEmotion,Double>();
    private EmotionTheory _theory;
    
    /**
     * interface for representing basic emotions
     */
    interface BasicEmotion{
        
        /**
         * retrieves the theory that the basic emotion is associated with
         */
        public EmotionTheory getTheory();
         
   }
    
    /**
     * Simple enumeration class to denote the availabe theories on emotions.
     */
    public enum EmotionTheory {

        PLUTCHIK, EKMAN; 
        
    }
    
    /**
     * retrieve all available emotion theories
     */
    public static EmotionTheory[] getAllEmotionTheories(){
        EmotionTheory[] symbols = new EmotionTheory[2];
        symbols = new EmotionTheory[] {EmotionTheory.PLUTCHIK, EmotionTheory.EKMAN};
        return symbols;
    }

    /**
     * Basic emotions according to (Plutchik, 1991).
     */
    public enum PlutchikBasicEmotion implements BasicEmotion {

        JOY, TRUST, FEAR, SURPRISE, SADNESS, DISGUST, ANGER, ANTICIPATION;
        
        public EmotionTheory theory = EmotionTheory.PLUTCHIK;
        
        public EmotionTheory getTheory(){return this.theory;}
   
   }
    
    
     public static PlutchikBasicEmotion[] getAllPlutchikBasicEmotions(){
        return PlutchikBasicEmotion.values();
   }
    

     /**
     * Basic emotions according to (Ekman, 1999).
     */
    public enum EkmanBasicEmotion implements BasicEmotion{

        AMUSEMENT, ANGER, CONTEMPT, CONTENTMENT, 
            DISGUST, EMBARASSMENT, EXCITEMENT, FEAR, GUILT, PRIDEINACHIEVEMENT, 
            RELIEF, SADNESSDISTRESS, SATISFACTION, SENSORYPLEASURE, SHAME;
        
        public EmotionTheory theory = EmotionTheory.EKMAN;
        
         public EmotionTheory getTheory(){return this.theory;}
        
    }
  
    
    /**
     * list all basic emotions, regardless from which theory
     */
    public static BasicEmotion[] listEmotions(EmotionTheory theory){
        BasicEmotion[] values = null;
        switch(theory){ 
            case PLUTCHIK: values = PlutchikBasicEmotion.values(); break;
            case EKMAN:    values = EkmanBasicEmotion.values(); break;
        }
        return values;
    }
     
     /**
     * indicate the basic emotion of opposite polarity (according to (Plutchik, 1991))
     */
    // still untested
    public static PlutchikBasicEmotion PlutchikInverseBasicEmotion(PlutchikBasicEmotion basic_emotion){
        PlutchikBasicEmotion inverse = null;
            switch (basic_emotion){
                case JOY: inverse = PlutchikBasicEmotion.SADNESS;  break;
                case SADNESS: inverse = PlutchikBasicEmotion.JOY;  break;
                case TRUST: inverse = PlutchikBasicEmotion.DISGUST;  break;
                case DISGUST: inverse = PlutchikBasicEmotion.TRUST;  break;
                case FEAR: inverse = PlutchikBasicEmotion.ANGER;  break;
                case ANGER: inverse = PlutchikBasicEmotion.FEAR;  break;
                case SURPRISE: inverse = PlutchikBasicEmotion.ANTICIPATION;  break;
                case ANTICIPATION: inverse = PlutchikBasicEmotion.SURPRISE;  break;       
            }
            return inverse;
    }
    
     /**
     * calculate the difference between two basic emotions in Plutchik's theory
     */
    public static double offsetEmotion(PlutchikBasicEmotion e1, PlutchikBasicEmotion e2, double v1, double v2){
        assert(e1 == PlutchikInverseBasicEmotion(e2));
        double val = java.lang.Math.abs(v1 - v2);
        return val;
    }
    
    
   private void setTheory(EmotionTheory theory){
       _theory = theory;
       return;
   }
    
    public EmotionTheory getTheory(){
        return _theory;
    }
    
     private HashMap<BasicEmotion,Double> getComponents(){
        return _components;
    }
     
     private void  setComponents(HashMap<BasicEmotion,Double> components){
        _components = components;
         return;
    }
    
    
    public Emotion(EmotionTheory theory){
        _theory = theory;
    }

     /**
     * construct an emotion based on one singe basic emotion
     */
     public Emotion(BasicEmotion basicemotion, double strength){
         assert (basicemotion !=null );
         _theory = basicemotion.getTheory();
        _components.put(basicemotion, strength);
        // _strength = strength;
    }
    
    
    /**
     * construct all available emotions that consist of one basic emotion each from the selected emotional theory
     */
    public static Emotion[] listPureEmotions(EmotionTheory theory, float strength){
        Double strength2 = Double.parseDouble(Float.toString(strength));
        BasicEmotion[] basic_emotion_list = listEmotions(theory);
        Emotion[] array = new Emotion[basic_emotion_list.length];
        int i = 0;
        for (BasicEmotion b : basic_emotion_list){
            array[i]= new Emotion(b, strength2);
            i++;
        }
        
        return array;
    }
    
  
    /**
     * indicate the percentage of one basic emotion within an emotion instance
     */
    public double getComponentValue (BasicEmotion basicemotion){
        double value = 0.0;
        if (_components.containsKey(basicemotion)){
            value =  _components.get(basicemotion);
        }
        return value;
    }
    
    public void putComponentValue (BasicEmotion basicemotion, double strength){
        _components.put(basicemotion,strength);
        return;
        
    }
    
    
    // OPERATIONS ON EMOTIONS
    
    /**
     * assuming that the argument is an emotion consisting of only one basic emotion, 
     * returns the object's value for that particular basic emotion
     */
    public Double project(Emotion pureemotion){
         HashMap targetcomponents = pureemotion.getComponents();
        // assert that emotions are both from matching theory
        assert (this._theory == pureemotion._theory); 
        assert (targetcomponents.size()==1.0);
        Set keys = targetcomponents.keySet();
        Iterator itr = keys.iterator();
        Double result =0.0;
        BasicEmotion targetcomponent = (BasicEmotion) itr.next();
        result =  _components.get(targetcomponent);
        if (result == null) {result = 0.0;} ;
        return result;
    }
   
     
    public static Emotion clone(Emotion original){
        // Object _newcomps = _components.clone();
        HashMap<BasicEmotion,Double> _newcomponents = new HashMap<BasicEmotion,Double>(original._components);
        EmotionTheory _newtheory = original._theory;
        Emotion newemotion = new Emotion(_newtheory);
        newemotion.setComponents(_newcomponents);
        return newemotion;
    }
    
    public void multiply(float factor){
         for(BasicEmotion basic : _components.keySet()){
                if (_components.get(basic)!= null)
                     {
                putComponentValue(basic, factor * _components.get(basic));
            }
       
        }
        return;
    }
    
     public static Emotion add(Emotion e1, Emotion e2){
         BasicEmotion[] basic_emotion_list = listEmotions(e1.getTheory());
         Emotion newemotion = new Emotion(e1.getTheory());
          // Object _newcomps = _components.clone();
         for(BasicEmotion basic : basic_emotion_list){
             double val1 = e1.getComponentValue(basic);
             double val2 = e2.getComponentValue(basic);
             newemotion.putComponentValue(basic,val1 + val2);      
        }
        return newemotion;
    }
    
  
    
      
    
 public String heartSym(){
      return new String("\u2764");
  }
  
  public String skullSym(){
      return new String("\u2620");
  }
  
  public String smileySym(){
      return new String("\u263a");
  }
  
  public String frowneySym(){
      return new String("\u2639");
  }
  
  public String flowerSym(){
      return new String("\u2698");
  }
  
   public String voltageSym(){
      return new String("\u26a1");
  }
   
   public String biohazSym(){
      return new String("\u2623");
  }
   
   public String downSym(){
      return new String("\u261f");
  }
   
     
    /**
     * Pretty-printing of emotions
     */
    public String display(){
        ArrayList<BasicEmotion> sortedemotions = new ArrayList<BasicEmotion>(_components.size()) ;
        String resultstring = "";
        
        // build ArrayList sorted by percentages
       
         for (BasicEmotion basicemotion: _components.keySet()) {
            if (sortedemotions.size() == 0 && _components.get(basicemotion) != 0){ // was >0
                sortedemotions.add(basicemotion);
            }
           else{
               boolean done = false;
               int index = sortedemotions.size();
               for (int i=0; i < sortedemotions.size(); i++){
                   BasicEmotion sortedbasicemotion = sortedemotions.get(i);
                   double current_perc = _components.get(basicemotion);
                   double running_perc = _components.get(sortedbasicemotion);
                   if (current_perc <= running_perc && done==false){
                       index = i;
                       done = true;
                   } 
               }
               
               if (_components.get(basicemotion) != 0) // >0
               { sortedemotions.add(index, basicemotion); }
               
            }          
        }
        
       for (BasicEmotion basicemotion: sortedemotions){
           String symbol = "";
           if (basicemotion.getTheory() == EmotionTheory.PLUTCHIK)
           {
               PlutchikBasicEmotion pbe = (PlutchikBasicEmotion) basicemotion;
                switch (pbe) {
                    case JOY: symbol     = smileySym(); break;
                    case FEAR: symbol = "f"; break;
                    case SADNESS: symbol = frowneySym(); break;
                    case SURPRISE: symbol = "!"; break;
                    case ANTICIPATION: symbol = flowerSym(); break;
                    case TRUST: symbol = heartSym(); break;
                    case ANGER: symbol = skullSym(); break;
                    case DISGUST: symbol = biohazSym(); break;
                }
           }
           if (basicemotion.getTheory() == EmotionTheory.EKMAN)
           {
               EkmanBasicEmotion pbe = (EkmanBasicEmotion) basicemotion;
                switch (pbe) {
                    case AMUSEMENT: symbol     = "amus."; break;
                    case ANGER: symbol = "ang."; break;
                    case CONTEMPT: symbol = "contempt"; break;
                    case CONTENTMENT: symbol = "contentm."; break;
                    case DISGUST: symbol = "disg."; break;
                    case EMBARASSMENT: symbol = "emb."; break;
                    case EXCITEMENT: symbol = "exc."; break;
                    case FEAR: symbol = "fear"; break;
                    case GUILT: symbol = "guilt"; break;
                    case PRIDEINACHIEVEMENT: symbol = "pr.ach."; break;
                    case RELIEF: symbol = "rel."; break;
                    case SADNESSDISTRESS: symbol = "sadn.dis."; break;
                    case SATISFACTION: symbol = "sat."; break;
                    case SENSORYPLEASURE: symbol = "sens.pleas."; break;
                    case SHAME: symbol = "shame"; break;
                                        
                }
           }
          
           java.text.DecimalFormat df = new java.text.DecimalFormat(".000");
           if (_components.get(basicemotion) < 0){}
            else{
           resultstring = symbol + String.valueOf(df.format(_components.get(basicemotion))) + " " + resultstring;
            }
        }
       
       return resultstring;
    }
    
    public String displayNONUTF(){
        ArrayList<BasicEmotion> sortedemotions = new ArrayList<BasicEmotion>(_components.size()) ;
        String resultstring = "";
        
        // build ArrayList sorted by percentages
        
        for (BasicEmotion basicemotion: _components.keySet()) {
            if (sortedemotions.size() == 0 && _components.get(basicemotion) != 0){  // was >0
                sortedemotions.add(basicemotion);
            }
            else{
                boolean done = false;
                int index = sortedemotions.size();
                for (int i=0; i < sortedemotions.size(); i++){
                    BasicEmotion sortedbasicemotion = sortedemotions.get(i);
                    double current_perc = _components.get(basicemotion);
                    double running_perc = _components.get(sortedbasicemotion);
                     if (current_perc <= running_perc && done==false){
                         index = i;
                         done = true;
                     } 
                }
                
                if (_components.get(basicemotion) != 0) //>0
                { sortedemotions.add(index, basicemotion); }
                
             }          
        }
        
        for (BasicEmotion basicemotion: sortedemotions){
             String symbol = "";
            if (basicemotion.getTheory() == EmotionTheory.PLUTCHIK)
            {
                PlutchikBasicEmotion pbe = (PlutchikBasicEmotion) basicemotion;
                switch (pbe) {
                    case JOY: symbol     = "joy"; break;
                    case FEAR: symbol = "fear"; break;
                    case SADNESS: symbol = "sad"; break;
                    case SURPRISE: symbol = "!"; break;
                    case ANTICIPATION: symbol = "anticip"; break;
                    case TRUST: symbol = "trust"; break;
                    case ANGER: symbol = "anger"; break;
                    case DISGUST: symbol = "disgust"; break;
                }
            }
            if (basicemotion.getTheory() == EmotionTheory.EKMAN)
            {
                EkmanBasicEmotion pbe = (EkmanBasicEmotion) basicemotion;
                switch (pbe) {
                    case AMUSEMENT: symbol     = "amus."; break;
                    case ANGER: symbol = "ang."; break;
                    case CONTEMPT: symbol = "contempt"; break;
                    case CONTENTMENT: symbol = "contentm."; break;
                    case DISGUST: symbol = "disg."; break;
                    case EMBARASSMENT: symbol = "emb."; break;
                    case EXCITEMENT: symbol = "exc."; break;
                     case FEAR: symbol = "fear"; break;
                    case GUILT: symbol = "guilt"; break;
                    case PRIDEINACHIEVEMENT: symbol = "pr.ach."; break;
                    case RELIEF: symbol = "rel."; break;
                    case SADNESSDISTRESS: symbol = "sadn.dis."; break;
                    case SATISFACTION: symbol = "sat."; break;
                    case SENSORYPLEASURE: symbol = "sens.pleas."; break;
                    case SHAME: symbol = "shame"; break;
                        
                }
            }
            // resultstring = symbol + String.valueOf(_components.get(basicemotion)) + " " + resultstring;
            java.text.DecimalFormat df = new java.text.DecimalFormat(".000");
            if (_components.get(basicemotion) < 0){}
            else{
                resultstring = symbol + " " + String.valueOf(df.format(_components.get(basicemotion))) + " " + resultstring;
            }
        }
        
        return resultstring;
        
    }
   

}
