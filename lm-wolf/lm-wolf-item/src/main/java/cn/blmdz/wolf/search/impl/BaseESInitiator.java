package cn.blmdz.wolf.search.impl;

import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.StringUtils;

import com.google.common.base.Charsets;
import com.google.common.io.Resources;

import cn.blmdz.home.search.ESClient;

public abstract class BaseESInitiator
{
  private static final Logger log = LoggerFactory.getLogger(BaseESInitiator.class);
  private final ESClient esClient;

  protected BaseESInitiator(ESClient esClient)
  {
    this.esClient = esClient;
  }

  public void init(String indexName, String indexType, String mappingPath)
    throws IOException
  {
    if (!this.esClient.health()) {
      log.warn("elasticsearch is not available ");
      return;
    }
    this.esClient.createIndexIfNotExists(indexName);
    if (!StringUtils.hasText(mappingPath)) {
      mappingPath = indexType + "_mapping.json";
    }
    String mapping = Resources.toString(Resources.getResource(mappingPath), Charsets.UTF_8);
    this.esClient.createMappingIfNotExists(indexName, indexType, mapping);
  }
}