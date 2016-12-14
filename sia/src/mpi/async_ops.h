/*
 * async_state.h
 *
 *  Created on: Jun 22, 2015
 *      Author: basbas
 */

#ifndef ASYNC_STATE_H_
#define ASYNC_STATE_H_

#include <list>
#include <mpi.h>
#include "sip.h"
#include "sip_mpi_constants.h"
#include "barrier_support.h"
#include "sip_mpi_utils.h"


namespace sip {

class ServerBlock;

/**
 * Base class for pending asynchronous operations on a block
 *
 * These objects are owned by a data structure belonging to the block and constructed when
 * inserted and destructed when removed.  These objects are not copyable--there should be
 * a single instance per operation.
 *
 * Semantic requirements and coding standards related to asynchronous operations:
 *
 * The constructor of each class should perform the asynchronous send
 * or post the asynchronous receive that derived objects of this class are handling.
 *
 * The do_handle method should perform tasks necessary to complete the operation
 * after it has been enabled.  This may require sending a message, removing
 * items from a data structure, or doing nothing (in the case the op is an
 * asynchronous mpi send).
 *
 * Closure: Subclasses should contain enough information for their methods
 * to be callable from any context.
 *
 * Nonblocking:  An enabled operation should be able to be executed to completion without
 * blocking.
 *
 *
 * This class is a wrapper for these operations which ensures that calls are idempotent.
 *
 *
 */
class AsyncBase {
public:
	enum AsyncStatus {
		WAITING = 1, READY = 2, DONE = 3,
	};

	AsyncBase(int pc) :
			async_status_(WAITING), pc_(pc) {
	}

	virtual ~AsyncBase() {
		CHECK(async_status_ == DONE,
				"attempting to deconstruct unhandled request");
	}

	/** Tests the status of this operation
	 *
	 * @return  true if the operation is enabled or completed, and false otherwise
	 */
	bool test() {
		if (async_status_ != WAITING)
			return true;
		if (do_test()) {
			async_status_ = READY;
			return true;
		}
		return false;
	}

	/**
	 * Waits for this operation to be enabled or completed.
	 * On return, async_status_ is READY or DONE.
	 */
	void wait() {
		if (async_status_ != WAITING)
			return;
		do_wait();
		async_status_ = READY;
	}

	bool is_done() {
		return async_status_ == DONE;
	}

	/**  Attempts to handle this operation.
	 *
	 * If the operation cannot be performed, then async_status remains WAITING and the method returns false
	 * If the operation can be performed, it is, asynch_status is set to DONE, and the method returns true
	 *
	 * @return  true if the operation has been completed and async_status_ set to DONE
	 */
	bool try_handle() {
		if (async_status_ == READY || ((async_status_ == WAITING) && test())) {
			do_handle();  //this should always succeed if called when enabled
			async_status_ = DONE;
			return true;
		}
		return async_status_ == DONE;
	}

	/** returns true if the asynchronous operation represented by this object
	 * may modify the block
	 * @return
	 */
	bool is_write() {
		return do_is_write();
	}

	friend std::ostream& operator<<(std::ostream& os, const AsyncBase &obj) {
		os << obj.to_string();
		return os;
	}

protected:
	//subclasses should invoke AsyncStatus::to_string() in overriding methods
	virtual std::string to_string() const {
		std::stringstream ss;
		ss << "async_status_=" << async_status_;
		ss << " pc_=" << pc_ << " ";
		return ss.str();
	}

private:
	AsyncStatus async_status_; //initially WAITING, must be DONE before destruction
	int pc_; //index into optable of the sialx instruction that generated this operation

	//precondition:  async_state == WAITNG
	//postcondition: returned value == "operation is enabled"
	virtual bool do_test()=0;

	//precondition:   async_state == WAITING
	//postcondition:  "operation is enabled"
	virtual void do_wait()=0;

	//precondition:  async_state == READY (or WAITING and test has returned true)
	//postcondition:  op completed
	virtual void do_handle()=0;

	//returns true if this operation may modify the block
	virtual bool do_is_write()=0;

	DISALLOW_COPY_AND_ASSIGN (AsyncBase);
};

/** Represents asynchronous send of block by server in response to get operation
 * Once the send operation is complete, no additional "handling" needs to be done.
 *
 * The block used in the operation should have been obtained by a call to
 * get_block_for reading, which should wait for any pending writes to complete.
 */
class GetAsync: public AsyncBase {
public:
	//asynchronous send with response performed in constructor.
	GetAsync(int mpi_source, int get_tag, ServerBlock* block, int pc);
	virtual ~GetAsync() {
	}

private:
	MPI_Request mpi_request_;

	bool do_test() {
		int flag = 0;
		MPI_Test(&mpi_request_, &flag, MPI_STATUS_IGNORE);
		return flag;
	}

	void do_wait() {
		MPI_Wait(&mpi_request_, MPI_STATUS_IGNORE);
	}

	void do_handle() {
		//nothing to do here since we have either returned from wait or test returned true
	}

	bool do_is_write() {
		return false;
	}

	std::string to_string() const {
		std::stringstream ss;
		ss << "GetAsync";
		ss << AsyncBase::to_string();
		return ss.str();
	}
	DISALLOW_COPY_AND_ASSIGN (GetAsync);
};

/** Represents asynchronous put_accumulate operation at server
 *
 * Instance will be created in response to put_accumulate message to handle
 * the second message containing the data and to perform the accumulate operation.
 *
 * Instances of this class own the temporary buffer used to receive the data.
 *
 * Constructor:
 *             allocates temporary buffer,
 *             posts Irecv for block data.
 *
 * do_handle: sends ack for data message to source
 *           performs the accumulate operation of temp data into block
 *
 * Destructor: deletes the temp data buffer.
 *
 *The constructor does not send the "reply" message to the source to let it know that
 *the data message can be sent.  This should be done AFTER this object is constructed
 *in order to ensure that the Irecv has been posted, and thus ensuring that the
 *large message can bypass the MPI buffers
 *
 *TODO:  reuse the temp buffers?
 *
 */
class PutAccumulateDataAsync: public AsyncBase {
public:
	PutAccumulateDataAsync(int mpi_source, int put_accumulate_data_tag,
			ServerBlock* block, int pc);
	virtual ~PutAccumulateDataAsync();
private:
	ServerBlock* block_;
	int mpi_source_;
	int tag_;
	MPI_Request mpi_request_;
	double* temp_; //buffer to receive data.  created in constructor, deleted in destructor

	bool do_test();
	void do_wait();
	void do_handle();
	bool do_is_write() {
		return true;
	}
	virtual std::string to_string() const;
	DISALLOW_COPY_AND_ASSIGN (PutAccumulateDataAsync);
};

/** Represents asynchronous put data operation at server.
 *
 * Will be created in response to put message.
 *
 * This class assumes that there is at most one unhandled put message per block at any time.
 * This property is guaranteed because the block should have been obtained by a call
 * to get_block_for_writing,  This method should call the block's
 * CommunicationState.wait method which waits for all pending ops to complete.
 *
 * Constructor: sends reply to source,
 *             posts Irecv for block data
 *             The receive buffer is the block's data array, which should exist already
 *
 * do_handle: sends ack for data message to source
 *
 *The constructor does not send the "reply" message to the source to let it know that
 *the data message can be sent.  This should be done AFTER this object is constructed
 *order to ensure that the Irecv has been posted (and thus ensuring that the message
 *bypasses MPI buffers
 *
 */
class PutDataAsync: public AsyncBase {

public:
	PutDataAsync(int mpi_source, int put_data_tag, ServerBlock* block, int pc);
	virtual ~PutDataAsync() {
	}

private:
	ServerBlock* block_;
	int mpi_source_;
	int tag_;
	MPI_Request mpi_request_;

	bool do_test();
	void do_wait();
	void do_handle();
	bool do_is_write() {
		return true;
	}
	std::string to_string() const;
	DISALLOW_COPY_AND_ASSIGN (PutDataAsync);
};

/** This class manages the pending asynchronous communication
 * operations on a single block.  It contains
 * a list of pending operations, and provides a set of factory methods
 * to create and save AsyncOp operations.
 *
 * Currently, get_block_for_reading should wait for all pending write  ops,
 * and and get_block_for_writing stop and wait for all pending operations to be completed before returning. get_block_for_accumulate,
 * on the other hand does not wait, but adds its async_op to the list
 * and returns.
 *
 * All asynchronous operations are handled in the order they are submitted to this list.
 *
 * Methods in this class are responsible for appropriately maintaining
 * the invariant that num_pending_writes_ is the number of ops in the
 * pending list which may modify the block (i.e. for which is_write()
 * will return true.)
 *
 * Each asynchronous operation, OP,  should have an add_OP method in this class which creates
 * an instance of an appropriate subclass of AsyncBase and adds it to the pending list.
 */
class ServerBlockAsyncManager {
public:
	ServerBlockAsyncManager():
		num_pending_writes_(0) {
	}
	~ServerBlockAsyncManager() {
		CHECK(pending_.empty(), "deleting block with pending async ops");
	}

	//increments num_pending_writes_
	void add_put_accumulate_data_request(int mpi_source,
			int put_accumulate_data_tag, ServerBlock* block, int pc);

	void add_put_data_request(int mpi_source, int put_data_tag,
			ServerBlock* block, int pc);

	void add_get_reply(int mpi_source, int get_tag, ServerBlock *, int pc);

	/**
	 *
	 * @return true if this block has pending operations
	 */
	bool has_pending() {
		return !pending_.empty();
	}

	/**
	 * Attempts to handle all pending items in the list, in order.  If an item is found that
	 * is not enabled, false is returned. Otherwise, the list will be empty and true is returned.
	 * On a return value of true, the caller should delete this block from its set of blocks with pending
	 * items.
	 *
	 * @return  false if the pending list for the block is not on return empty.
	 *
	 */
	bool try_handle_all_test_none_pending() {
		std::list<AsyncBase*>::iterator it = pending_.begin();
		while (it != pending_.end()) {
			bool res = (*it)->try_handle();
			if (res) {
				if ((*it)->is_write()) {
					num_pending_writes_--;
				}
				delete *it;
				it = pending_.erase(it);
			} else
				return false;
		}
		return true;
	}

	/**
	 * Attempts to handle the first item in the list.  If the first item is not enabled, the routine
	 * returns immediately.  Otherwise, the item will be handled and removed from the list.  If the
	 * first item is actually done, it will be removed from the list, and the next item tried.
	 * If the list is empty, or becomes empty after removing done operations, true is returned,
	 * otherwise false.
	 *
	 * @return  false if the pending list for the block is not empty after attempting to handle one op.
	 *
	 * From the point of view of the caller, try_handle_all and try_handle look the
	 * same, a return value of true means that all pending ops for this block have
	 * been handled.  The difference is how long the methods keep trying.
	 *
	 */
	bool try_handle_test_none_pending() {
		std::list<AsyncBase*>::iterator it = pending_.begin();
		//remove already done ops, if any, from list
		while (it != pending_.end() && (*it)->is_done()) {
			if ((*it)->is_write()) {
				num_pending_writes_--;
			}
			delete *it;
			it = pending_.erase(it);
		}
		if (it == pending_.end()) {
			//nothing left to handle
			return true;
		}
		//try to handle first op
		bool res = (*it)->try_handle();
		if (res) {
			if ((*it)->is_write()) {
				num_pending_writes_--;
			}
			delete *it;
			it = pending_.erase(it);
		}
		return pending_.empty();

	}

	/** Handles all pending asyncs on this block, waiting for each one in
	 * turn to be enabled.
	 *
	 * Postcondition:  there are no pending ops on this block
	 * */

	void wait_all() {
		std::list<AsyncBase*>::iterator it = pending_.begin();
		while (it != pending_.end()) {
			(*it)->wait();
			bool res = (*it)->try_handle();
			CHECK(res, "in BlockAsyncManager::wait--handle ready op failed");
			if ((*it)->is_write()) {
				num_pending_writes_--;
			}
			delete *it;
			it = pending_.erase(it);
		}
	}

	/** Handles all pending writes on this block, waiting for each one in
	 * turn to be enabled.  Since ops must be handled in order, any pending
	 * reads preceding a write in the list are also awaited
	 *
	 * Postcondition:  there are no pending write ops on this block
	 * */
	void wait_for_writes() {
		std::list<AsyncBase*>::iterator it = pending_.begin();
		while (it != pending_.end() && num_pending_writes_>0) {
			(*it)->wait();
			bool res = (*it)->try_handle();
			CHECK(res, "in BlockAsyncManager::wait--handle ready op failed");
			if ((*it)->is_write()) {
				num_pending_writes_--;
			}
			delete *it;
			it = pending_.erase(it);
		}
	}

private:
	std::list<AsyncBase*> pending_;
	int num_pending_writes_;
	DISALLOW_COPY_AND_ASSIGN (ServerBlockAsyncManager);

};

} /* namespace sip */

#endif /* ASYNC_STATE_H_ */
