// Copyright (c) 2011-2012, Marvin Schiller,
// with contributions by Peter C. R. Lane.
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.architecture;

import java.io.*;
import java.lang.Math;
import java.util.*;

/**
 * Represents a complex emotional tag
 *
 * @author Marvin Schiller
 */
public class EmotionalTrace {
    
    // private float _strength;
    // private Node source; not needed, is in the association list
    private Map<Integer,Emotion> _emotionHistory = new HashMap<Integer,Emotion>();
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
        Set<Integer> sorted_timepoints = new TreeSet<Integer>(timepoints);
        for (Integer i : sorted_timepoints){
            System.out.print("t=");
            System.out.print(i);
            System.out.print(": ");
            System.out.println(_emotionHistory.get(i).displayNONUTF());
        }
        return;
    }
    

}
