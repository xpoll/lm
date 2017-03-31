package cn.blmdz.home.search.api.model;

import com.google.common.base.MoreObjects;
import com.google.common.base.Throwables;

import cn.blmdz.home.search.api.Indexer;

import java.io.Serializable;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class BulkIndexTask implements Runnable, Serializable {
	private static final Logger log = LoggerFactory
			.getLogger(BulkIndexTask.class);
	private static final long serialVersionUID = 3153641360660043996L;
	private final Indexer indexer;
	private final String indexName;
	private final String indexType;
	private final String templatePath;
	private final List params;

	public BulkIndexTask(Indexer indexer, String indexName, String indexType,
			String templatePath, List params) {
		this.indexer = indexer;
		this.indexName = indexName;
		this.indexType = indexType;
		this.templatePath = templatePath;
		this.params = params;
	}

	public void run() {
		try {
			this.indexer.bulk(this.indexName, this.indexType,
					this.templatePath, this.params);
		} catch (Exception var2) {
			log.error(
					"failed to bulk index (indexName={}, indexType={}, params:{}) cause:{}",
					new Object[] { this.indexName, this.indexType, this.params,
							Throwables.getCausalChain(var2) });
		}

	}

	public String toString() {
		return MoreObjects.toStringHelper(this)
				.add("indexName", this.indexName)
				.add("indexType", this.indexType)
				.add("templatePath", this.templatePath)
				.add("params", this.params).omitNullValues().toString();
	}
}
