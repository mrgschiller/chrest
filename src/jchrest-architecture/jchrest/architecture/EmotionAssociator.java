// Copyright (c) 2011-2012, Marvin Schiller,
// with contributions by Peter C. R. Lane.
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.architecture;

import java.io.*;
import java.lang.Math;
import java.util.*;
import java.util.HashMap;

/**
 * A mechanism for managing the associations between nodes and emotions
 *
 * @author Marvin Schiller
 */

public class EmotionAssociator{
    
    private HashMap<Node,EmotionalTrace>  _associations = new HashMap<Node,EmotionalTrace>();
    private double _default_alpha = 0.2; // this is arbitrary
    
    public void addEmotionalTrace(Node node, EmotionalTrace trace){
        _associations.put(node,trace);
    }
    
    public void setDefaultAlpha(double alpha){
        _default_alpha = alpha;
        return;
    }
    
    private double alpha(Node cue,Emotion.BasicEmotion basic){
        return _default_alpha;
    }
          
    private static double lambda(Node node, Emotion emotion, Emotion.BasicEmotion basic){
        return emotion.getComponentValue(basic);
    }
        
     /**
     * Retrieve an emotional trace object for a node
     */
    public EmotionalTrace getEmotionalTrace(Node node){
        EmotionalTrace trace = _associations.get(node);
        // if (trace == null){
        //    trace = new EmotionalTrace();
        // }
        return trace;
    }
    
    /**
     * Retrieve the associated emotional response for a node
     */
    public Emotion getRWEmotion(Node node){
        EmotionalTrace trace = getEmotionalTrace(node);
        if (trace==null){
            return null; 
        }
        assert(trace != null);
        Emotion emotion = trace.getRescorlaWagnerEmotion();
        return emotion;
    }
    
    /**
     * Set the conditioned emotional response associated to a node to a specific value
     */
    public void setRWEmotion(Node node, Emotion emotion){
        EmotionalTrace trace = getEmotionalTrace(node);
        if (trace == null){
            addEmotionalTrace(node,new EmotionalTrace());}
        trace = getEmotionalTrace(node);
        trace.setRescorlaWagnerEmotion(emotion);
        return;
    }
    
     /**
     * Retrieve most recent emotion for each STM, and propagate those to all other nodes still in STM.
     */
    public void emoteAndPropagateAcrossModalities(Stm[] stms, int time){
       
        for(Stm stm : stms){
            ArrayList<Node> cues = new ArrayList<Node>();
            int stm_size = stm.getCount();
            if (stm_size !=0){
                Node topnode = stm.getItem(0);
                EmotionalTrace trace = _associations.get(topnode);
                if (trace == null){return;}
                Emotion current_emotion = trace.getRescorlaWagnerEmotion();
               if (!(current_emotion == null)){
                   // spread out to all stms
                   for (int i=0; i < stms.length; i++){
                      Stm target_stm = stms[i];
                        // loop for target stm items, should include all (including top item)
                       for (int j=0; j < target_stm.getCount(); j++ ){
                           Node node = target_stm.getItem(j);
                           if (!(stm == stms[i] && j==0) && !(node.getReference()==0)) // exclude the item from propagating to itself, and exclude the root node (assumed to carry reference number 0). 
                           {
                               cues.add(node);                              
                           }
                       }
                   }
               }
               
               learnEmotion(current_emotion, topnode,cues,time);  
            }
        }
        return;
    }
    
     /**
     * A service function for emoteAndPropagateAcrossModalities, where cues are related to a particular target emotion
     */
    public void learnEmotion(Emotion emotion, Node node, ArrayList<Node> cues, int time){
        if (cues==null){return; }
        // for each cue, lern the given emotion and add to history
        for (Node cue: cues){
            // 1. add emotion to history
            EmotionalTrace trace = getEmotionalTrace(cue);
            if (trace == null){
                _associations.put(cue,new EmotionalTrace());
                trace =  getEmotionalTrace(cue);
            }    
            trace.addToHistory(time, emotion);
            addEmotionalTrace(cue, trace);
        }
        // 2. Compute Rescorla Wagner Emotion
        computeRW(emotion, node, cues); // potential to compute vAll beforehand
        return;
    }
    
    /**
     * A service function for computeRW
     */
    public Emotion computeVAll(ArrayList<Node> cues){
        ArrayList<Emotion> emotions = new ArrayList<Emotion>();
        for(Node cue: cues){
            EmotionalTrace trace = _associations.get(cue);
            if (trace !=null){
                Emotion RWemotion = trace.getRescorlaWagnerEmotion();
                if (RWemotion !=null){
                    emotions.add(RWemotion);
                }
            }
        }
        if (emotions.isEmpty()){return null; } 
        else {
            // for each basic emotion compute sum
            Emotion.EmotionTheory theory = emotions.get(0).getTheory();
            for (Emotion emotion: emotions){
             assert (theory == emotion.getTheory());
            }
            Emotion.BasicEmotion[] basicemotions = Emotion.listEmotions(theory);
            Emotion newemotion = new Emotion(theory);
            for (Emotion.BasicEmotion basic: basicemotions){
                // sum up from all cues
                double strength = 0;
                for (Emotion emotion: emotions){
                    strength = strength + emotion.getComponentValue(basic);
                }
                newemotion.putComponentValue(basic, strength);
            }            
            return newemotion;       
        }       
    }
    
     /**
     * Compute emotion according to Rescorla Wagner Update function on all dimensions
     */
    public void computeRW(Emotion emotion, Node node, ArrayList<Node> cues){
        Emotion vAll = computeVAll(cues);
        if (vAll==null){
            vAll = new Emotion(emotion.getTheory());
        }     
        Emotion.EmotionTheory theory = emotion.getTheory();
        Emotion.BasicEmotion[] basicemotions = Emotion.listEmotions(theory);
        for (Node cue : cues){
            Emotion newemotion = new Emotion(theory);
            EmotionalTrace trace = getEmotionalTrace(cue);
            if (trace==null){
                _associations.put(cue, new EmotionalTrace());
                trace = _associations.get(cue);
            }
            for(Emotion.BasicEmotion basic : basicemotions){
                double vallComponent =  vAll.getComponentValue(basic);
                double deltaV = alpha(cue,basic) * (lambda(node,emotion,basic) - vallComponent);
                double V = 0.0;
                if (trace.getRescorlaWagnerEmotion() != null){ 
                    V = trace.getRescorlaWagnerEmotion().getComponentValue(basic); 
                }
                double newV = V + deltaV;
                newemotion.putComponentValue(basic,newV);
            }
            trace.setRescorlaWagnerEmotion(newemotion);           
        }
        return;
    }
        
    
    /**
     * Service function for debugging
     */
    public void emotionTraceToStdOut(Node node){
        EmotionalTrace trace = _associations.get(node);
        if (trace == null){
            System.out.print("Emotional trace for node ");
            System.out.print(node.getReference());
            System.out.println(" is empty");
            return;
        }
        else{
            System.out.println("Printing emotional trace for node ");
            System.out.println(node.getReference());
            trace.printToStdOut();
            return;
        }
        
    }
}
