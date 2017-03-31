package cn.blmdz.home.session.serialize;

import cn.blmdz.home.session.exception.SerializeException;
import cn.blmdz.home.session.serialize.Serializer;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.JavaType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Throwables;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class JsonSerializer implements Serializer {
   private static final Logger log = LoggerFactory.getLogger(JsonSerializer.class);
   private final ObjectMapper objectMapper = new ObjectMapper();
   private final JavaType javaType;

   public JsonSerializer() {
      this.objectMapper.setSerializationInclusion(Include.NON_EMPTY);
      this.javaType = this.objectMapper.getTypeFactory().constructParametricType(Map.class, new Class[]{String.class, Object.class});
   }

   public String serialize(Object o) {
      try {
         return this.objectMapper.writeValueAsString(o);
      } catch (Exception var3) {
         log.error("failed to serialize http session {} to json,cause:{}", o, Throwables.getStackTraceAsString(var3));
         throw new SerializeException("failed to serialize http session to json", var3);
      }
   }

   public Map deserialize(String o) {
      try {
         return (Map)this.objectMapper.readValue(o, this.javaType);
      } catch (Exception var3) {
         log.error("failed to deserialize string  {} to http session,cause:{} ", o, Throwables.getStackTraceAsString(var3));
         throw new SerializeException("failed to deserialize string to http session", var3);
      }
   }
}
