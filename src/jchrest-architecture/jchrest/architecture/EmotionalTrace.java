// Copyright (c) 2011-2012, Marvin Schiller,
// with contributions by Peter C. R. Lane.
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.architecture;

import java.io.*;
import java.lang.Math;
import java.util.*;
import java.util.HashMap;


/**
 * Represents a complex emotional tag (for one memory item), including a history of emotions encountered, and an emotion representing a conditioned emotional response (the Rescorla Wagner emotion)
 *
 * @author Marvin Schiller
 */
 
public class EmotionalTrace {
    
   
    private HashMap<Integer,Emotion> _emotionHistory = new HashMap<Integer,Emotion>();
    private Emotion _RescorlaWagnerEmotion;
    
    public void addToHistory(int time, Emotion emotion){
        _emotionHistory.put(time,emotion);
    } 
    
    public void setRescorlaWagnerEmotion(Emotion emotion){
        _RescorlaWagnerEmotion = emotion;
    }
    
    public Emotion getRescorlaWagnerEmotion(){
        return _RescorlaWagnerEmotion;
    } 
    
    
    /**
     * Pretty-printing for debugging purposes
     */
    
    public void printToStdOut(){
        System.out.println("== Emotional Trace ==");
        System.out.println("= RW Emotion =");
        if (_RescorlaWagnerEmotion==null){
            System.out.println("empty");
        }
        else{
            System.out.println(_RescorlaWagnerEmotion.displayNONUTF());
        }
        System.out.println("= Emotion History =");
        Set<Integer> timepoints = _emotionHistory.keySet();
        TreeSet<Integer> sorted_timepoints = new TreeSet<Integer>(timepoints);
        for (Integer i : sorted_timepoints){
            System.out.print("t=");
            System.out.print(i);
            System.out.print(": ");
            System.out.println(_emotionHistory.get(i).displayNONUTF());
        }
        return;
    }
    

}
