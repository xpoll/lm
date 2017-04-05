package io.terminus.parana.search.impl;

import com.google.common.base.Charsets;
import com.google.common.io.Resources;
import io.terminus.parana.search.item.SearchItemProperties;
import io.terminus.search.core.ESClient;
import javax.annotation.PostConstruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

@Component
public class ESItemInitiator {
   private static final Logger log = LoggerFactory.getLogger(ESItemInitiator.class);
   private final ESClient esClient;
   private final SearchItemProperties searchItemProperties;

   @Autowired
   public ESItemInitiator(ESClient esClient, SearchItemProperties searchItemProperties) {
      this.esClient = esClient;
      this.searchItemProperties = searchItemProperties;
   }

   @PostConstruct
   public void init() throws Exception {
      if(!this.esClient.health()) {
         log.warn("elasticsearch is not available ");
      } else {
         this.esClient.createIndexIfNotExists(this.searchItemProperties.getIndexName());
         String mappingPath = this.searchItemProperties.getMappingPath();
         if(!StringUtils.hasText(mappingPath)) {
            mappingPath = this.searchItemProperties.getIndexType() + "_mapping.json";
         }

         String mapping = Resources.toString(Resources.getResource(mappingPath), Charsets.UTF_8);
         this.esClient.createMappingIfNotExists(this.searchItemProperties.getIndexName(), this.searchItemProperties.getIndexType(), mapping);
      }
   }
}
