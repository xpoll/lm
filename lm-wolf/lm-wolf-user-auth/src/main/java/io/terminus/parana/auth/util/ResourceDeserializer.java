package io.terminus.parana.auth.util;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;
import io.terminus.parana.auth.model.CategoryResource;
import io.terminus.parana.auth.model.RequestResource;
import io.terminus.parana.auth.model.Resource;
import io.terminus.parana.auth.util.YAML;
import java.io.IOException;

public class ResourceDeserializer extends JsonDeserializer {
   public Resource deserialize(JsonParser p, DeserializationContext ctxt) throws IOException, JsonProcessingException {
      JsonNode node = (JsonNode)p.getCodec().readTree(p);
      JsonNode requests = node.get("requests");
      return requests == null?(Resource)YAML.mapper().convertValue(node, CategoryResource.class):(Resource)YAML.mapper().convertValue(node, RequestResource.class);
   }
}
