package cn.blmdz.aide.pay.driver;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.thoughtworks.xstream.converters.Converter;
import com.thoughtworks.xstream.converters.MarshallingContext;
import com.thoughtworks.xstream.converters.UnmarshallingContext;
import com.thoughtworks.xstream.io.HierarchicalStreamReader;
import com.thoughtworks.xstream.io.HierarchicalStreamWriter;

public class PojoMapConverter implements Converter {
   public boolean canConvert(Class clazz) {
      String classname = clazz.getName();
      return classname.indexOf("Map") >= 0 || classname.indexOf("List") >= 0 || classname.indexOf("Bean") >= 0;
   }

   public void marshal(Object value, HierarchicalStreamWriter writer, MarshallingContext context) {
      this.map2xml(value, writer, context);
   }

   protected void map2xml(Object value, HierarchicalStreamWriter writer, MarshallingContext context) {
      boolean bMap = true;
      String classname = value.getClass().getName();
      bMap = classname.indexOf("List") < 0;
      if(bMap) {
         Map<String, Object> map = (Map)value;

         for(Entry<String, Object> entry : map.entrySet()) {
            String key = (String)entry.getKey();
            Object subvalue = entry.getValue();
            writer.startNode(key);
            if(subvalue instanceof Map) {
               this.map2xml(subvalue, writer, context);
            } else {
               writer.setValue(subvalue.toString());
            }

            writer.endNode();
         }
      } else {
         for(Object subval : (List)value) {
            writer.startNode("child");
            if(subval.getClass().getName().indexOf("String") >= 0) {
               writer.setValue((String)subval);
            } else {
               this.map2xml(subval, writer, context);
            }

            writer.endNode();
         }
      }

   }

   public Map unmarshal(HierarchicalStreamReader reader, UnmarshallingContext context) {
      Map<String, Object> map = (Map)this.populateMap(reader, context);
      return map;
   }

   protected Object populateMap(HierarchicalStreamReader reader, UnmarshallingContext context) {
      boolean bMap = true;
      Map<String, Object> map = Maps.newTreeMap();

      List<Object> list;
      for(list = Lists.newArrayList(); reader.hasMoreChildren(); reader.moveUp()) {
         reader.moveDown();
         String key = reader.getNodeName();
         Object value = null;
         if(reader.hasMoreChildren()) {
            value = this.populateMap(reader, context);
         } else {
            value = reader.getValue();
         }

         if(!bMap) {
            list.add(value);
         } else if(!map.containsKey(key)) {
            map.put(key, value);
         } else {
            bMap = false;
            Iterator<Entry<String, Object>> iter = map.entrySet().iterator();

            while(iter.hasNext()) {
               list.add(((Entry)iter.next()).getValue());
            }

            list.add(value);
         }
      }

      if(bMap) {
         return map;
      } else {
         return list;
      }
   }
}
