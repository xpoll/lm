package cn.blmdz.home.search.utils;

import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;

public class JsonUtil {
   public static final ObjectMapper JSON_NON_EMPTY_MAPPER = (new JsonUtil(Include.NON_EMPTY)).getMapper();
   private final ObjectMapper mapper = new ObjectMapper();

   private JsonUtil(Include include) {
      this.mapper.setSerializationInclusion(include);
      this.mapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);
   }

   public ObjectMapper getMapper() {
      return this.mapper;
   }
}
