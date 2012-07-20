// Copyright (c) 2011-2012, Marvin Schiller,
// with contributions by Peter C. R. Lane.
// Released under Open Works License, http://owl.apotheon.org/

package jchrest.architecture;

import java.io.*;
import java.lang.Math;
import java.util.*;

/**
 * A mechanism for managing the associations between nodes and emotions
 *
 * @author Marvin Schiller
 */
public class EmotionAssociator{
    
    private Map<Node,EmotionalTrace>  _associations = new HashMap<Node,EmotionalTrace>();
    private double _default_alpha = 0.2;
    
    public void addEmotionalTrace(Node node, EmotionalTrace trace){
        _associations.put(node,trace);
    }
    
    public void setDefaultAlpha(double alpha){
        _default_alpha = alpha;
        return;
    }
    
    
    public EmotionalTrace getEmotionalTrace(Node node){
        EmotionalTrace trace = _associations.get(node);
        // if (trace == null){
        //    trace = new EmotionalTrace();
        // }
        return trace;
    }
    
    public Emotion getRWEmotion(Node node){
        EmotionalTrace trace = getEmotionalTrace(node);
        if (trace==null){
            return null; 
        }
        assert(trace != null);
        Emotion emotion = trace.getRescorlaWagnerEmotion();
        if (emotion == null){
            System.out.println("getRWEmotion -- produced null emotion!");
        }
        return emotion;
    }
    
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
    public void emoteAndPropagateAcrossModalities(Stm[] stms, int time) {

      for(Stm stm : stms){
        List<Node> cues = new ArrayList<Node>();
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
                  //addEmotionalLink(node, current_emotion, _stability_increment); 

                }
              }
            }
          }

          learnEmotion(current_emotion, topnode,cues,time);  
        }
      }
      return;
    }
    
    public void learnEmotion(Emotion emotion, Node node, List<Node> cues, int time){
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
    
    public Emotion computeVAll(List<Node> cues){
        List<Emotion> emotions = new ArrayList<Emotion>();
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
    
    public void computeRW(Emotion emotion, Node node, List<Node> cues){
        Emotion vAll = computeVAll(cues);
        if (vAll==null){
            vAll = new Emotion(emotion.getTheory());
        }
        System.out.print("vAll: ");                                            //
        System.out.println(vAll);                                              //
        Emotion.EmotionTheory theory = emotion.getTheory();
        Emotion.BasicEmotion[] basicemotions = Emotion.listEmotions(theory);
        for (Node cue : cues){
            System.out.print("looping with node: ");                            //
            System.out.println(cue.getReference());                             //
            Emotion newemotion = new Emotion(theory);
            EmotionalTrace trace = getEmotionalTrace(cue);
            if (trace==null){
                _associations.put(cue, new EmotionalTrace());
                trace = _associations.get(cue);
                 System.out.println("added new emotional trace");              //
            }
            for(Emotion.BasicEmotion basic : basicemotions){
                 System.out.print("looping with emotion ");                      //
                System.out.println(basic);                                       //
                double vallComponent =  vAll.getComponentValue(basic);
                  System.out.print("vallComponent: ");                         //
                 System.out.print("lambda");                                   //
                System.out.println(lambda(node,emotion,basic));                //
                double deltaV = alpha(cue,basic) * (lambda(node,emotion,basic) - vallComponent);
                 System.out.print("deltaV: ");                                  //
                 System.out.println(deltaV);                                    //
                double V = 0.0;
                if (trace.getRescorlaWagnerEmotion() != null){ 
                    V = trace.getRescorlaWagnerEmotion().getComponentValue(basic); 
                }
                double newV = V + deltaV;
                 System.out.print("newV: ");                                     //
                System.out.println(newV);                                        //
                newemotion.putComponentValue(basic,newV);
            }
            trace.setRescorlaWagnerEmotion(newemotion);           
        }
        return;
    }
    
    private double alpha(Node cue,Emotion.BasicEmotion basic){
        return _default_alpha;
    }
    
    
    
    private static double lambda(Node node, Emotion emotion, Emotion.BasicEmotion basic){
        return emotion.getComponentValue(basic);
    }
    
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
