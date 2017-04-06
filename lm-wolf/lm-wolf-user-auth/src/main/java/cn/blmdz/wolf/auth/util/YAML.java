package cn.blmdz.wolf.auth.util;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.module.SimpleModule;
import com.fasterxml.jackson.dataformat.yaml.YAMLFactory;

import cn.blmdz.wolf.auth.model.Resource;
import cn.blmdz.wolf.auth.util.ResourceDeserializer;

public class YAML {
   private static ObjectMapper mapper;

   public static ObjectMapper mapper() {
      if(mapper == null) {
         mapper = new ObjectMapper(new YAMLFactory());
         mapper.setSerializationInclusion(Include.NON_NULL);
         mapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
         mapper.configure(SerializationFeature.WRITE_DATES_AS_TIMESTAMPS, false);
         mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
         mapper.configure(DeserializationFeature.ACCEPT_EMPTY_STRING_AS_NULL_OBJECT, true);
         mapper.configure(DeserializationFeature.ACCEPT_SINGLE_VALUE_AS_ARRAY, true);
         SimpleModule module = new SimpleModule("parana auth module");
         module.addDeserializer(Resource.class, new ResourceDeserializer());
         mapper.registerModule(module);
      }

      return mapper;
   }
}
