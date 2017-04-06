package cn.blmdz.wolf.auth.util;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;

import cn.blmdz.wolf.auth.model.CategoryResource;
import cn.blmdz.wolf.auth.model.RequestResource;
import cn.blmdz.wolf.auth.model.Resource;
import cn.blmdz.wolf.auth.util.YAML;

import java.io.IOException;

public class ResourceDeserializer extends JsonDeserializer {
   public Resource deserialize(JsonParser p, DeserializationContext ctxt) throws IOException, JsonProcessingException {
      JsonNode node = (JsonNode)p.getCodec().readTree(p);
      JsonNode requests = node.get("requests");
      return requests == null?(Resource)YAML.mapper().convertValue(node, CategoryResource.class):(Resource)YAML.mapper().convertValue(node, RequestResource.class);
   }
}
