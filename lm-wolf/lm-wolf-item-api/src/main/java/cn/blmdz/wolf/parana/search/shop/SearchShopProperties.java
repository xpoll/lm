package cn.blmdz.wolf.parana.search.shop;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "shop.search")
public class SearchShopProperties {
	private String indexName;
	private String indexType;
	private String mappingPath;
	private Integer fullDumpRange;
	private Integer batchSize;

	public SearchShopProperties() {
		this.fullDumpRange = Integer.valueOf(3);

		this.batchSize = Integer.valueOf(100);
	}

	public String getIndexName() {
		return this.indexName;
	}

	public String getIndexType() {
		return this.indexType;
	}

	public String getMappingPath() {
		return this.mappingPath;
	}

	public Integer getFullDumpRange() {
		return this.fullDumpRange;
	}

	public Integer getBatchSize() {
		return this.batchSize;
	}

	public void setIndexName(String indexName) {
		this.indexName = indexName;
	}

	public void setIndexType(String indexType) {
		this.indexType = indexType;
	}

	public void setMappingPath(String mappingPath) {
		this.mappingPath = mappingPath;
	}

	public void setFullDumpRange(Integer fullDumpRange) {
		this.fullDumpRange = fullDumpRange;
	}

	public void setBatchSize(Integer batchSize) {
		this.batchSize = batchSize;
	}

}