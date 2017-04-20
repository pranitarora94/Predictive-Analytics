import java.awt.List;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.conf.Configured;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.lib.output.TextOutputFormat;
import org.apache.hadoop.util.Tool;
import org.apache.hadoop.util.ToolRunner;



public class AdvProg  extends Configured implements Tool  {

	public static void main(String[] args) throws Exception {
		// TODO Auto-generated method stub
		System.out.println(Arrays.toString(args));
	      int res = ToolRunner.run(new Configuration(), new AdvProg(), args);
	      
	      System.exit(res);
	}
	@Override
	   public int run(String[] args) throws Exception {
	      System.out.println(Arrays.toString(args));
	      Job job = new Job(getConf(), "WordCount");
	      job.setJarByClass(AdvProg.class);
	      job.setOutputKeyClass(Text.class);
	      job.setOutputValueClass(IntWritable.class);

	      job.setMapperClass(Map.class);
	      job.setReducerClass(Reduce.class);

	      job.setInputFormatClass(TextInputFormat.class);
	      job.setOutputFormatClass(TextOutputFormat.class);

	      FileInputFormat.addInputPath(job, new Path(args[0]));
	      FileOutputFormat.setOutputPath(job, new Path(args[1]));

	      job.waitForCompletion(true);
	      
	      return 0;
	   } 
	   public static class Map extends Mapper<LongWritable, Text, Text, IntWritable> {
	      private IntWritable userID = new IntWritable(1);
	      private Text temp = new Text();
	      
	      private ArrayList<String> customers = new ArrayList<String>();
	      
	      
	      private void Combinations(int index, int numberElemRemain, String temp1, IntWritable uid, Context context)
	      throws IOException, InterruptedException
	      {
	    	  if(numberElemRemain ==0)
	    	  {
	    		  Text myText = new Text(temp1);
	    		  context.write(myText, uid);
	    		  //System.out.println(temp1);
	    		  return ;
	    	  }
	    	  //System.out.println(customers.size());
	    	  if(index>=customers.size())
	    	  {
	    		  return;
	    	  }
	    	  //System.out.println(i);
	    	  if(customers.size()-numberElemRemain >= index)
	   			  Combinations(index+1, numberElemRemain, temp1, uid, context);
	   		  temp1 = temp1 + "," + customers.get(index);
	   		  Combinations(index+1, numberElemRemain - 1, temp1, uid, context);
	   	  }
	      
	      @Override
	      public void map(LongWritable key, Text value, Context context)
	              throws IOException, InterruptedException {
	    	  int i = 0;
	    	  //System.out.println(value);
	    	  customers = new ArrayList<String>();
	    	  String temp2 = new String();
	         for (String token: value.toString().split("\\s+")) {
	            if(i ==0)
	            {
	            	userID.set(Integer.parseInt(token.substring(1)));
	            }
	            else
	            {
	            	for(String t1: token.split(","))
	            	{
	            		customers.add(t1);
	            		//System.out.println(t1);
	            	}
	            }
	            i++;
	         }for(int j=1; j <=customers.size(); j++)
	         {
	        	Combinations(0, j, temp2, userID, context);
	         }
	      }
	   }
	   public static class Reduce extends Reducer<Text, IntWritable, Text, Text> {
	      @Override
	      public void reduce(Text key, Iterable<IntWritable> values, Context context)
	              throws IOException, InterruptedException {
	         
	         HashMap<String, Integer> MyMap = new HashMap<String, Integer>();
	         //Integer myInt = new IntWritable(1);
	         String ToWrite = "";
         
	         for (IntWritable val : values) {
	        	 String userId = "u" + val.toString();
	        	 if(MyMap.containsKey(userId))
	            {
	            	MyMap.put(userId, MyMap.get(userId) + 1);
	            }
	            else
	            {
	            	MyMap.put(userId, 1);
	            }
	            
	         }
	         ToWrite = "";
	         for(String uid : MyMap.keySet())
	         {
	        	 ToWrite = ToWrite + uid + "(" + MyMap.get(uid).toString() + "), ";
	         }
	         ToWrite = ToWrite.substring(0, ToWrite.length()-2);
	         String temp = key.toString().substring(1);
	         key = new Text(temp);
	         context.write(key, new Text(ToWrite));
	      } 
	   }

}



   

